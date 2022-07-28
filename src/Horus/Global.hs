module Horus.Global
  ( GlobalT (..)
  , GlobalF (..)
  , Config (..)
  , solveContract
  , SolvingInfo (..)
  )
where

import Control.Monad (when)
import Control.Monad.Except (MonadError (..), MonadTrans, lift)
import Control.Monad.Trans.Free.Church (FT, liftF)
import Data.Function ((&))
import Data.Map qualified as Map (toList)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text (intercalate)
import Data.Traversable (for)
import Lens.Micro (at, non, (^.))
import Lens.Micro.GHC ()

import Horus.CFGBuild (CFGBuildT, Label, LabeledInst, buildCFG)
import Horus.CFGBuild.Runner (CFG (..))
import Horus.CairoSemantics (CairoSemanticsT, encodeSemantics)
import Horus.CairoSemantics.Runner
  ( ConstraintsState (..)
  , MemoryVariable (..)
  , debugFriendlyModel
  , makeModel
  )
import Horus.ContractDefinition (Checks, ContractDefinition (..), cPreConds, cdChecks)
import Horus.ContractInfo (ContractInfo (..), mkContractInfo)
import Horus.HReference (HReference (..), evalReference)
import Horus.Label (Label (..))
import Horus.Module (Module, nameOfModule, runModuleL, traverseCFG)
import Horus.Preprocessor (PreprocessorL, SolverResult (..), solve)
import Horus.Preprocessor.Runner (PreprocessorEnv (..))
import Horus.Preprocessor.Solvers (Solver, SolverSettings)
import Horus.Program (Identifiers, Program (..))
import Horus.SMTUtil.Transpiler (exprToSMT)
import Horus.SW.AST (CairoType (..), Expr (..))
import Horus.SW.Identifier (Identifier (..), Reference (..), ReferenceData (..), getFunctionPc)
import Horus.SW.Instruction (labelInsructions, readAllInstructions)
import Horus.Util (tShow)
import SimpleSMT.Typed qualified as SMT (TSExpr (True))

data Config = Config
  { cfg_verbose :: Bool
  , cfg_solver :: Solver
  , cfg_solverSettings :: SolverSettings
  }

data GlobalF m a
  = forall b. RunCFGBuildT (CFGBuildT m b) (CFG -> a)
  | forall b. RunCairoSemanticsT ContractInfo (CairoSemanticsT m b) (ConstraintsState -> a)
  | AskConfig (Config -> a)
  | PutStrLn' Text a
  | forall b. RunPreprocessor PreprocessorEnv (PreprocessorL b) (b -> a)
  | Throw Text
  | forall b. Catch (GlobalT m b) (Text -> GlobalT m b) (b -> a)

deriving instance Functor (GlobalF m)

newtype GlobalT m a = GlobalT {runGlobalT :: FT (GlobalF m) m a}
  deriving newtype (Functor, Applicative, Monad)

instance MonadTrans GlobalT where
  lift = GlobalT . lift

instance MonadError Text (GlobalT m) where
  throwError = throw
  catchError = catch

liftF' :: GlobalF m a -> GlobalT m a
liftF' = GlobalT . liftF

runCFGBuildT :: CFGBuildT m a -> GlobalT m CFG
runCFGBuildT cfgBuilder = liftF' (RunCFGBuildT cfgBuilder id)

runCairoSemanticsT :: ContractInfo -> CairoSemanticsT m a -> GlobalT m ConstraintsState
runCairoSemanticsT env smt2Builder = liftF' (RunCairoSemanticsT env smt2Builder id)

askConfig :: GlobalT m Config
askConfig = liftF' (AskConfig id)

runPreprocessor :: PreprocessorEnv -> PreprocessorL a -> GlobalT m a
runPreprocessor penv preprocessor =
  liftF' (RunPreprocessor penv preprocessor id)

putStrLn' :: Text -> GlobalT m ()
putStrLn' what = liftF' (PutStrLn' what ())

throw :: Text -> GlobalT m a
throw t = liftF' (Throw t)

catch :: GlobalT m a -> (Text -> GlobalT m a) -> GlobalT m a
catch m h = liftF' (Catch m h id)

verbosePutStrLn :: Text -> GlobalT m ()
verbosePutStrLn what = do
  config <- askConfig
  when (cfg_verbose config) (putStrLn' what)

verbosePrint :: Show a => a -> GlobalT m ()
verbosePrint what = verbosePutStrLn (tShow what)

makeCFG :: Checks -> Identifiers -> (Label -> CFGBuildT m Label) -> [LabeledInst] -> GlobalT m CFG
makeCFG checks identifiers labelToFun labeledInsts =
  runCFGBuildT (buildCFG checks identifiers labelToFun labeledInsts)

makeModules :: ContractDefinition -> CFG -> GlobalT m [Module]
makeModules cd cfg = pure (runModuleL (traverseCFG sources cfg))
 where
  sources = cd_program cd & p_identifiers & Map.toList & mapMaybe takeSourceAndPre
  preConds = cd ^. cdChecks . cPreConds
  takeSourceAndPre (name, idef) = do
    pc <- getFunctionPc idef
    let pre = preConds ^. at name . non SMT.True
    pure (pc, pre)

makeReferences :: ContractDefinition -> GlobalT m [HReference]
makeReferences cd = do
  let identifiers = Map.toList $ p_identifiers (cd_program cd)
  let cairoReferences = [ref | (IReference ref) <- snd <$> identifiers]
  refs <- for cairoReferences $ \Reference{..} ->
    for ref_references $ \ReferenceData{..} -> do
      smtExpr <- case (ref_cairoType, rd_value) of
        (TypeStruct _name, Deref value) -> exprToSMT value
        _ -> exprToSMT rd_value
      -- smtExpr <- exprToSMT rd_value
      pure $ HReference (tShow ref_name) ref_cairoType smtExpr (Label rd_pc)
  pure $ concat refs

extractConstraints :: [HReference] -> ContractInfo -> Module -> GlobalT m ConstraintsState
extractConstraints references env m = runCairoSemanticsT env (encodeSemantics references m)

data SolvingInfo = SolvingInfo
  { si_moduleName :: Text
  , si_result :: SolverResult
  , si_refs :: Text
  }

solveModule :: [HReference] -> ContractInfo -> Text -> Module -> GlobalT m SolvingInfo
solveModule references contractInfo smtPrefix m = do
  result <- mkResult
  tRefs <- case result of
    Sat (Just model) ->
      Text.intercalate "\n"
        <$> ( for references $ \ref -> do
                case evalReference model (ci_identifiers contractInfo) ref of
                  Left e -> pure e
                  Right t -> pure $ href_name ref <> " " <> t
            )
    _ -> pure ""
  pure
    SolvingInfo
      { si_moduleName = moduleName
      , si_result = result
      , si_refs = tRefs
      }
 where
  mkResult = printingErrors $ do
    verbosePrint m
    constraints <- extractConstraints references contractInfo m
    verbosePrint (debugFriendlyModel constraints)
    solveSMT smtPrefix constraints
  printingErrors a = a `catchError` (\e -> pure (Unknown (Just ("Error: " <> e))))
  moduleName = nameOfModule (ci_identifiers contractInfo) m

solveSMT :: Text -> ConstraintsState -> GlobalT m SolverResult
solveSMT smtPrefix cs = do
  Config{..} <- askConfig
  runPreprocessor (PreprocessorEnv memVars cfg_solver cfg_solverSettings refs) (solve query)
 where
  query = makeModel smtPrefix cs
  memVars = map (\mv -> (mv_varName mv, mv_addrName mv)) (cs_memoryVariables cs)
  refs = fst <$> cs_references cs

solveContract :: Monad m => ContractDefinition -> GlobalT m [SolvingInfo]
solveContract cd = do
  insts <- readAllInstructions (p_code (cd_program cd))
  let labeledInsts = labelInsructions insts
  verbosePrint labeledInsts
  cfg <- makeCFG checks identifiers getFunPc labeledInsts
  verbosePrint cfg
  references <- makeReferences cd
  modules <- makeModules cd cfg
  for modules (solveModule references contractInfo (cd_rawSmt cd))
 where
  contractInfo = mkContractInfo cd
  getFunPc = ci_getFunPc contractInfo
  identifiers = p_identifiers (cd_program cd)
  checks = cd_checks cd
