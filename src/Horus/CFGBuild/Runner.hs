module Horus.CFGBuild.Runner
  ( CFG (..)
  , interpret
  , runImpl
  , cfgArcs
  )
where

import Control.Monad.Except (ExceptT, catchError, runExceptT, throwError)
import Control.Monad.Free.Church (iterM)
import Control.Monad.Reader (ReaderT, ask, asks, runReaderT)
import Control.Monad.State (State, runState, get)
import Data.List (union)
import Data.Map (Map)
import Data.Map qualified as Map (empty)
import Data.Text (Text)
import Data.Text qualified as Text (pack)
import Lens.Micro (Lens', at, (&), (^.), _Just)
import Lens.Micro.GHC ()
import Lens.Micro.Mtl ((%=), (<%=))

import Horus.CFGBuild (ArcCondition (..), CFGBuildF (..), CFGBuildL (..), LabeledInst, AnnotationType, Vertex (..))
import Horus.ContractInfo (ContractInfo (..))
import Horus.Expr (Expr, Ty (..))
import Horus.FunctionAnalysis (FInfo)

type Impl = ReaderT ContractInfo (ExceptT Text (State CFG))

data CFG = CFG
  { cfg_vertices :: [Vertex]
  , cfg_arcs :: Map Vertex [(Vertex, [LabeledInst], ArcCondition, FInfo)]
  , cfg_assertions :: Map Vertex [(AnnotationType, Expr TBool)]
  , cfg_vertexCounter :: Int
  }
  deriving (Show)

emptyCFG :: CFG
emptyCFG = CFG [] Map.empty Map.empty 0

cfgVertices :: Lens' CFG [Vertex]
cfgVertices lMod g = fmap (\x -> g{cfg_vertices = x}) (lMod (cfg_vertices g))

cfgArcs :: Lens' CFG (Map Vertex [(Vertex, [LabeledInst], ArcCondition, FInfo)])
cfgArcs lMod g = fmap (\x -> g{cfg_arcs = x}) (lMod (cfg_arcs g))

cfgAssertions :: Lens' CFG (Map Vertex [(AnnotationType, Expr TBool)])
cfgAssertions lMod g = fmap (\x -> g{cfg_assertions = x}) (lMod (cfg_assertions g))

cfgVertexCounter :: Lens' CFG Int
cfgVertexCounter lMod g = fmap (\x -> g{cfg_vertexCounter = x}) (lMod (cfg_vertexCounter g))

interpret :: CFGBuildL a -> Impl a
interpret = iterM exec . runCFGBuildL
 where
  exec (AddVertex l cont) = do
    freshVal <- cfgVertexCounter <%= succ
    let newVertex = Vertex (Text.pack (show freshVal)) l
    cfgVertices %= ([newVertex] `union`)
    cont newVertex
  exec (AddArc lFrom lTo insts test isF cont) = cfgArcs . at lFrom %= doAdd >> cont
   where
    doAdd mArcs = Just ((lTo, insts, test, isF) : mArcs ^. _Just)
  exec (AddAssertion l assertion cont) = cfgAssertions . at l %= doAdd >> cont
   where
    doAdd mAssertions = Just (assertion : mAssertions ^. _Just)
  exec (AskIdentifiers cont) = asks ci_identifiers >>= cont
  exec (AskProgram cont) = asks ci_program >>= cont
  exec (GetFuncSpec name cont) = do
    ci <- ask
    ci_getFuncSpec ci name & cont
  exec (GetInvariant name cont) = do
    ci <- ask
    ci_getInvariant ci name & cont
  exec (GetRets name cont) = do
    ci <- ask
    ci_getRets ci name >>= cont
  exec (GetVerts l cont) = do
    cfg <- get
    cont [v | v <- cfg_vertices cfg, v_label v == l]
  exec (Throw t) = throwError t
  exec (Catch m handler cont) = catchError (interpret m) (interpret . handler) >>= cont

runImpl :: ContractInfo -> Impl a -> Either Text CFG
runImpl contractInfo m = do
  let (r, cfg) = runReaderT m contractInfo & runExceptT & flip runState emptyCFG
  fmap (const cfg) r
