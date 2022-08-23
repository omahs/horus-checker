module Horus.CFGBuild
  ( CFGBuildT (..)
  , ArcCondition (..)
  , addAssertion
  , throw
  , buildCFG
  , Label (..)
  , CFGBuildF (..)
  , LabeledInst
  )
where

import Control.Monad.Except (MonadError (..), MonadTrans, lift)
import Control.Monad.Trans.Free.Church (FT, liftF)
import Data.Coerce (coerce)
import Data.Foldable (for_, toList)
import Data.Function ((&))
import Data.List (sort, union)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty (last, reverse, (<|))
import Data.Map (Map)
import Data.Map qualified as Map (elems, fromListWith, toList)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Lens.Micro (at, ix, non, (^.))
import Lens.Micro.GHC ()

import Horus.ContractDefinition (Checks (..))
import Horus.Instruction
  ( Instruction (..)
  , LabeledInst
  , OpCode (..)
  , PcUpdate (..)
  , getNextPc
  , jumpDestination
  )
import Horus.Label (Label (..), moveLabel)
import Horus.Program (Identifiers)
import Horus.SW.Identifier (getFunctionPc, getLabelPc)
import Horus.ScopedTSExpr (ScopedTSExpr, emptyScopedTSExpr)
import Horus.Util (Box (..), appendList, topmostStepFT, whenJust)

data ArcCondition = ACNone | ACJnz Label Bool
  deriving stock (Show)

data CFGBuildF a
  = AddVertex Label a
  | AddArc Label Label [LabeledInst] ArcCondition a
  | AddAssertion Label (ScopedTSExpr Bool) a
  | Throw Text
  deriving (Functor)

newtype CFGBuildT m a = CFGBuildT {runCFGBuildT :: FT CFGBuildF m a}
  deriving newtype (Functor, Applicative, Monad, MonadTrans)

liftF' :: CFGBuildF a -> CFGBuildT m a
liftF' = CFGBuildT . liftF

addVertex :: Label -> CFGBuildT m ()
addVertex l = liftF' (AddVertex l ())

addArc :: Label -> Label -> [LabeledInst] -> ArcCondition -> CFGBuildT m ()
addArc lFrom lTo insts test = liftF' (AddArc lFrom lTo insts test ())

addAssertion :: Label -> ScopedTSExpr Bool -> CFGBuildT m ()
addAssertion l assertion = liftF' (AddAssertion l assertion ())

throw :: Text -> CFGBuildT m a
throw t = liftF' (Throw t)

instance Monad m => MonadError Text (CFGBuildT m) where
  throwError = throw
  catchError m handler = do
    step <- lift (topmostStepFT (runCFGBuildT m))
    case step of
      Just (Box (Throw t)) -> handler t
      _ -> m

buildCFG ::
  Checks -> Identifiers -> (Label -> CFGBuildT m Label) -> [LabeledInst] -> CFGBuildT m ()
buildCFG checks identifiers getFunPc labeledInsts = do
  buildFrame labeledInsts identifiers
  retsByFun <- mapFunsToRets getFunPc labeledInsts
  addAssertions retsByFun checks identifiers

newtype Segment = Segment (NonEmpty LabeledInst)
  deriving (Show)

segmentLabel :: Segment -> Label
segmentLabel (Segment ((l, _) :| _)) = l

nextSegmentLabel :: Segment -> Label
nextSegmentLabel s = getNextPc (NonEmpty.last (coerce s))

segmentInsts :: Segment -> [LabeledInst]
segmentInsts (Segment ne) = toList ne

isControlFlow :: Instruction -> Bool
isControlFlow i = i_opCode i == Call || i_pcUpdate i /= Regular

buildFrame :: [LabeledInst] -> Identifiers -> CFGBuildT m ()
buildFrame rows identifiers = do
  let segments = breakIntoSegments labels rows
  for_ segments $ \s -> do
    addVertex (segmentLabel s)
    addArcsFrom s
 where
  funLabels = identifiers & Map.elems & mapMaybe getFunctionPc & coerce
  namedLabels = identifiers & Map.elems & mapMaybe getLabelPc & coerce
  jumpLabels = concat [[jmpDst, getNextPc i] | i <- rows, Just jmpDst <- [jumpDestination i]]
  retLabels = [pc | (pc, inst) <- rows, isRet inst]
  labels = sort (funLabels `union` namedLabels `union` jumpLabels `union` retLabels)

breakIntoSegments :: [Label] -> [LabeledInst] -> [Segment]
breakIntoSegments _ [] = []
breakIntoSegments ls_ (i_ : is_) = coerce (go [] (i_ :| []) ls_ is_)
 where
  go gAcc lAcc [] rest = reverse (NonEmpty.reverse lAcc `appendList` rest : gAcc)
  go gAcc lAcc (_ : _) [] = reverse (NonEmpty.reverse lAcc : gAcc)
  go gAcc lAcc (l : ls) (i@(pc, _) : is)
    | l < pc = go gAcc lAcc ls (i : is)
    | l == pc = go (NonEmpty.reverse lAcc : gAcc) (i :| []) ls is
    | otherwise = go gAcc (i NonEmpty.<| lAcc) (l : ls) is

addArc' :: Label -> Label -> [LabeledInst] -> CFGBuildT m ()
addArc' lFrom lTo insts = addArc lFrom lTo insts ACNone

addArcsFrom :: Segment -> CFGBuildT m ()
addArcsFrom s
  | not (isControlFlow endInst) = do
      let lTo = nextSegmentLabel s
      addArc' lFrom lTo insts
  | Call <- i_opCode endInst = do
      let lTo = nextSegmentLabel s
      addArc' lFrom lTo insts
  | Ret <- i_opCode endInst = do
      pure ()
  | JumpAbs <- i_pcUpdate endInst = do
      let lTo = Label (fromInteger (i_imm endInst))
      addArc' lFrom lTo (init insts)
  | JumpRel <- i_pcUpdate endInst = do
      let lTo = moveLabel endPc (fromInteger (i_imm endInst))
      addArc' lFrom lTo (init insts)
  | Jnz <- i_pcUpdate endInst = do
      let lTo1 = nextSegmentLabel s
          lTo2 = moveLabel endPc (fromInteger (i_imm endInst))
      addArc lFrom lTo1 insts (ACJnz endPc False)
      addArc lFrom lTo2 insts (ACJnz endPc True)
  | otherwise = pure ()
 where
  lFrom = segmentLabel s
  (endPc, endInst) = NonEmpty.last (coerce s)
  insts = segmentInsts s

addAssertions :: Map Label [Label] -> Checks -> Identifiers -> CFGBuildT m ()
addAssertions retsByFun checks identifiers = do
  for_ (Map.toList identifiers) $ \(idName, def) -> do
    whenJust (getFunctionPc def) $ \pc -> do
      let post = c_postConds checks ^. at idName . non emptyScopedTSExpr
      for_ (retsByFun ^. ix pc) (`addAssertion` post)
    whenJust (getLabelPc def) $ \pc ->
      whenJust (c_invariants checks ^. at idName) (pc `addAssertion`)

{- | Map each function label to a list of pcs of its 'rets'.

 Note, there might be no rets in a function, for example, when it ends with an endless
 loop.
-}
mapFunsToRets :: (Label -> CFGBuildT m Label) -> [LabeledInst] -> CFGBuildT m (Map Label [Label])
mapFunsToRets getFunPc rows = do
  retAndFun <- sequenceA [fmap (,[pc]) (getFunPc pc) | (pc, inst) <- rows, isRet inst]
  pure (Map.fromListWith (++) retAndFun)

isRet :: Instruction -> Bool
isRet Instruction{i_opCode = Ret} = True
isRet _ = False
