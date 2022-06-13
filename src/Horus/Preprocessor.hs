{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Horus.Preprocessor (Model (..), SolverResult (..), fetchModelFromSolver) where

import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor (bimap)
import Data.Foldable (foldlM, traverse_)
import Data.List (sortOn)
import Data.Map (Map, fromList, toList)
import Data.Maybe (catMaybes)
import Data.Text as Text (Text, pack, unpack)
import Data.Traversable (for)

import Lens.Micro (Lens')
import Lens.Micro.Mtl (view)

import Horus.Preprocessor.Solvers (Solver (..))
import Horus.Util (toSignedFelt)

import qualified SimpleSMT as SMT
import Z3.Base (Goal, Tactic)
import qualified Z3.Base (Model, modelTranslate)
import Z3.Monad (MonadZ3)
import qualified Z3.Monad as Z3

data SolverResult = Unsat | Sat Model | Unknown Text
data Model = Model
  { m_regs :: [(Text, Integer)]
  , m_mem :: Map Integer Integer
  }

instance Show SolverResult where
  show Unsat = "Unsat"
  show (Sat model) = "Sat\n" <> show model
  show (Unknown reason) = "Unknown\n" <> unpack reason

instance Show Model where
  show Model{..} =
    concatMap showAp m_regs
      <> concatMap showMem (toList m_mem)
   where
    showAp (reg, value) = unpack reg <> "\t\t=\t" <> show value <> "\n"
    showMem (addr, value) =
      "mem["
        <> show addr
        <> "]\t\t=\t"
        <> show value
        <> "\n"

data PreprocessorEnv = PreprocessorEnv
  { pe_memsAndAddrs :: [(String, String)]
  , pe_solver :: String -> IO (SMT.Result, Maybe String)
  }

peMemsAndAddrs :: Lens' PreprocessorEnv [(String, String)]
peMemsAndAddrs lMod g = fmap (\x -> g{pe_memsAndAddrs = x}) (lMod (pe_memsAndAddrs g))

peSolver :: Lens' PreprocessorEnv (String -> IO (SMT.Result, Maybe String))
peSolver lMod g = fmap (\x -> g{pe_solver = x}) (lMod (pe_solver g))

newtype PreprocessorT m a = PreprocessorT (ReaderT PreprocessorEnv m a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader PreprocessorEnv
    , MonadIO
    , MonadZ3
    )

runPreprocessorT :: PreprocessorT m a -> PreprocessorEnv -> m a
runPreprocessorT (PreprocessorT m) = runReaderT m

fetchModelFromSolver :: MonadZ3 z3 => Solver -> [(Text, Text)] -> Text -> z3 SolverResult
fetchModelFromSolver solver memVars expr = do
  goal <- sexprToGoal (unpack expr)
  runPreprocessorT
    (fetchModelFromSolver' goal)
    PreprocessorEnv
      { pe_memsAndAddrs = bimap unpack unpack <$> memVars
      , pe_solver = \sexpr -> (fmap . fmap) unpack <$> runSolver solver (pack sexpr)
      }

fetchModelFromSolver' :: MonadZ3 z3 => Goal -> PreprocessorT z3 SolverResult
fetchModelFromSolver' goal = do
  subgoals <- preprocess goal
  sgoals <- traverse goalToSExpr subgoals
  externalSolver <- view peSolver
  results <- liftIO $ traverse externalSolver sgoals
  let satGoals = [(subgoal, model) | ((SMT.Sat, Just model), subgoal) <- zip results subgoals]
  case satGoals of
    [] -> pure Unsat
    (subgoal, model) : _ -> mkFullModel subgoal model

mkFullModel :: MonadZ3 z3 => Goal -> String -> PreprocessorT z3 SolverResult
mkFullModel goal smodel = do
  realContext <- Z3.getContext
  model' <- Z3.local $ do
    context <- Z3.getContext
    Z3.solverFromString smodel
    _ <- Z3.solverCheck -- smodel consists of (declare-fun ...) expressions
    -- so should be okay to ignore the result.
    model <- Z3.solverGetModel
    liftIO $ Z3.Base.modelTranslate context model realContext
  fullModel <- Z3.convertModel goal model'
  model <- z3ModelToHorusModel fullModel
  pure $ Sat model

z3ModelToHorusModel :: MonadZ3 z3 => Z3.Base.Model -> PreprocessorT z3 Model
z3ModelToHorusModel model =
  Model
    <$> do
      consts <- Z3.getConsts model
      apsMb <- for consts $ \constDecl -> do
        nameSymbol <- Z3.getDeclName constDecl
        name <- Z3.getSymbolString nameSymbol
        interpRegVar name constDecl
      pure $
        sortOn
          ((\case x : _ : ind -> (x, read ind :: Integer); _ -> ('\0', 0)) . unpack . fst)
          (catMaybes apsMb)
    <*> do
      memVars <- view peMemsAndAddrs
      addrValueList <- for memVars $ \(memName, addrName) -> do
        memVar <- Z3.mkIntVar =<< Z3.mkStringSymbol memName
        addrVar <- Z3.mkIntVar =<< Z3.mkStringSymbol addrName
        mbValue <- Z3.modelEval model memVar True
        mbAddr <- Z3.modelEval model addrVar True
        case (mbAddr, mbValue) of
          (Just addrAst, Just valueAst) -> do
            addr <- Z3.getInt addrAst
            value <- Z3.getInt valueAst
            pure (addr, toSignedFelt value)
          _ -> liftIO $ fail "This was supposed to be unreachable"
      pure $ fromList addrValueList
 where
  interpRegVar :: MonadZ3 z3 => String -> Z3.FuncDecl -> z3 (Maybe (Text, Integer))
  interpRegVar name constDecl
    | firstLetter : 'p' : ind <- name
    , '+' `notElem` ind
    , firstLetter `elem` ['a', 'f'] = do
        mbVal <- Z3.getConstInterp model constDecl
        case mbVal of
          Nothing -> liftIO $ fail "This was supposed to be unreachable"
          Just val -> do
            intVal <- Z3.getInt val
            pure $ Just (pack name, toSignedFelt intVal)
    | otherwise = pure Nothing

mkMagicTactic :: MonadZ3 z3 => z3 Tactic
mkMagicTactic = do
  skip <- Z3.mkTactic "skip"
  tactics <- traverse Z3.mkTactic ["simplify", "solve-eqs", "propagate-values", "simplify"]
  foldlM Z3.andThenTactic skip tactics

goalToSExpr :: MonadZ3 z3 => Goal -> z3 String
goalToSExpr goal =
  Z3.local $
    Z3.getGoalFormulas goal
      >>= Z3.mkAnd
      >>= Z3.solverAssertCnstr
      >>= const Z3.solverToString

sexprToGoal :: MonadZ3 z3 => String -> z3 Goal
sexprToGoal sexpr = do
  goal <-
    Z3.mkGoal
      True -- enable model generation
      True -- enable unsat cores
      False -- disable proofs
  exprs <- Z3.parseSMTLib2String sexpr [] [] [] []
  traverse_ (Z3.goalAssert goal) exprs
  pure goal

preprocess :: MonadZ3 z3 => Goal -> z3 [Goal]
preprocess goal =
  mkMagicTactic >>= (`Z3.applyTactic` goal) >>= Z3.getApplyResultSubgoals
