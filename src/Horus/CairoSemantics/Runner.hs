{-# LANGUAGE RecordWildCards #-}

module Horus.CairoSemantics.Runner
  ( SemanticsEnv (..)
  , runT
  , run
  , ConstraintsState (..)
  , makeModel
  , debugFriendlyModel
  )
where

import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.State (MonadState, StateT, execStateT)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Free.Church (iterTM)
import Data.Function ((&))
import Data.Functor.Identity (runIdentity)
import qualified Data.List as List (find, tails, union)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as Text (intercalate)
import Data.Tuple.Select (sel1, sel2, sel3)
import Lens.Micro (Lens', at, ix, non, (%~), (<&>), (^.), (^?!))
import Lens.Micro.GHC ()
import Lens.Micro.Mtl (use, (%=), (<%=))

import Horus.CFGBuild (Label)
import Horus.CairoSemantics (CairoSemanticsF (..), CairoSemanticsL, CairoSemanticsT)
import Horus.Program (ApTracking)
import Horus.SMTUtil (prime)
import Horus.Util (tShow)
import SimpleSMT.Typed (TSExpr, showTSStmt, (.->), (.<), (.<=), (.==))
import qualified SimpleSMT.Typed as SMT (TSExpr (True), assert, const, declareInt, ppTSExpr, showTSExpr)

data ConstraintsState = ConstraintsState
  { cs_memoryVariables :: [(Text, Text, TSExpr Integer)]
  , cs_exprs :: [TSExpr Bool]
  , cs_decls :: [Text]
  , cs_nameCounter :: Int
  }

csMemoryVariables :: Lens' ConstraintsState [(Text, Text, TSExpr Integer)]
csMemoryVariables lMod g = fmap (\x -> g{cs_memoryVariables = x}) (lMod (cs_memoryVariables g))

csExprs :: Lens' ConstraintsState [TSExpr Bool]
csExprs lMod g = fmap (\x -> g{cs_exprs = x}) (lMod (cs_exprs g))

csDecls :: Lens' ConstraintsState [Text]
csDecls lMod g = fmap (\x -> g{cs_decls = x}) (lMod (cs_decls g))

csNameCounter :: Lens' ConstraintsState Int
csNameCounter lMod g = fmap (\x -> g{cs_nameCounter = x}) (lMod (cs_nameCounter g))

emptyConstraintsState :: ConstraintsState
emptyConstraintsState =
  ConstraintsState
    { cs_memoryVariables = []
    , cs_exprs = []
    , cs_decls = []
    , cs_nameCounter = 0
    }

data SemanticsEnv = SemanticsEnv
  { se_pres :: Map Label (TSExpr Bool)
  , se_posts :: Map Label (TSExpr Bool)
  , se_apTracking :: Map Label ApTracking
  }

newtype ImplT m a = ImplT (ReaderT SemanticsEnv (StateT ConstraintsState m) a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader SemanticsEnv
    , MonadState ConstraintsState
    )

instance MonadTrans ImplT where
  lift = ImplT . lift . lift

interpret :: forall m a. Monad m => CairoSemanticsT m a -> ImplT m a
interpret = iterTM exec
 where
  exec :: CairoSemanticsF (ImplT m a) -> ImplT m a
  exec (Assert a cont) = csExprs %= (a :) >> cont
  exec (DeclareFelt name cont) = do
    csDecls %= List.union [name]
    cont (SMT.const name)
  exec (DeclareMem address cont) = do
    memVars <- use csMemoryVariables
    case List.find ((address ==) . sel3) memVars of
      Just (name, _, _) -> cont (SMT.const name)
      Nothing -> do
        freshCount <- csNameCounter <%= (+ 1)
        let name = "MEM!" <> tShow freshCount
        let addrName = "ADDR!" <> tShow freshCount
        csMemoryVariables %= ((name, addrName, address) :)
        cont (SMT.const name)
  exec (GetPreByCall label cont) = do
    pres <- asks se_pres
    cont (pres ^. at label . non SMT.True)
  exec (GetPostByCall label cont) = do
    posts <- asks se_posts
    cont (posts ^. at label . non SMT.True)
  exec (GetApTracking label cont) = do
    trackings <- asks se_apTracking
    cont (trackings ^?! ix label)

debugFriendlyModel :: ConstraintsState -> Text
debugFriendlyModel ConstraintsState{..} =
  Text.intercalate "\n" (memoryPairs <> map SMT.ppTSExpr cs_exprs)
 where
  memoryPairs = [name <> "=[" <> SMT.showTSExpr addr <> "]" | (name, _, addr) <- cs_memoryVariables]

makeModel :: Text -> ConstraintsState -> Text
makeModel rawSmt ConstraintsState{..} =
  let names = "prime" : cs_decls <> map sel1 cs_memoryVariables <> map sel2 cs_memoryVariables
      decls = map SMT.declareInt names
      feltRestrictions = concat [[0 .<= SMT.const x, SMT.const x .< prime] | x <- tail names]
      memRestrictions = concatMap restrictMemTail (List.tails cs_memoryVariables)
      addrDefinitions = [SMT.const addrVar .== addr | (_, addrVar, addr) <- cs_memoryVariables]
      restrictions = concat [feltRestrictions, memRestrictions, addrDefinitions, cs_exprs]
   in (decls <> map SMT.assert restrictions)
        & map showTSStmt
        & (rawSmt :)
        & Text.intercalate "\n"
 where
  restrictMemTail [] = []
  restrictMemTail ((var0, _, addr0) : rest) =
    [addr0 .== addr .-> SMT.const var0 .== SMT.const var | (var, _, addr) <- rest]

runImplT :: Monad m => SemanticsEnv -> ImplT m a -> m ConstraintsState
runImplT env (ImplT m) = runReaderT m env & flip execStateT emptyConstraintsState

runT :: Monad m => SemanticsEnv -> CairoSemanticsT m a -> m ConstraintsState
runT env a = do
  cs <- runImplT env (interpret a)
  pure cs
    <&> csMemoryVariables %~ reverse
    <&> csExprs %~ reverse
    <&> csDecls %~ reverse

run :: SemanticsEnv -> CairoSemanticsL a -> ConstraintsState
run env = runIdentity . runT env
