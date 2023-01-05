{-# LANGUAGE InstanceSigs #-}
module Horus.CFGBuild
  ( CFGBuildL (..)
  , ArcCondition (..)
  , addAssertion
  , throw
  , buildCFG
  , Label (..)
  , CFGBuildF (..)
  , LabeledInst
  , AnnotationType(..)
  , mkPre
  , mkPost
  , mkInv
  , Vertex (..)
  , getVerts
  )
where

import Control.Arrow (Arrow(second))
import Control.Monad (when, unless)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Free.Church (F, liftF)
import Data.Coerce (coerce)
import Data.Foldable (forM_, for_, toList)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty (last, reverse, (<|))
import Data.Map ((!))
import Data.Map qualified as Map (toList)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Traversable (for)
import Lens.Micro.GHC ()

import Horus.Expr (Expr (), Ty (..))
import Horus.Expr qualified as Expr
import Horus.FunctionAnalysis
  ( FInfo
  , FuncOp (ArcCall, ArcRet)
  , ScopedFunction (ScopedFunction, sf_pc)
  , callersOf
  , pcToFunOfProg
  , programLabels
  , sizeOfCall, uncheckedScopedFOfPc
  )
import Horus.Instruction
  ( Instruction (..)
  , LabeledInst
  , OpCode (..)
  , PcUpdate (..)
  , getNextPc
  , uncheckedCallDestination
  )
import Horus.Label (Label (..), moveLabel)
import Horus.Program (Identifiers, Program (..))
import Horus.SW.FuncSpec (FuncSpec' (fs'_post, fs'_pre))
import Horus.SW.Identifier (Function (fu_pc), Identifier (IFunction, ILabel))
import Horus.SW.ScopedName (ScopedName)
import Horus.Util (appendList, whenJustM, tShow)
import Horus.Expr.Util (gatherLogicalVariables)

-- import Debug.Trace (traceM)

data AnnotationType = APre | APost | AInv
  deriving stock (Show)

mkPre :: Expr TBool -> (AnnotationType, Expr TBool)
mkPre = (APre,)

mkPost :: Expr TBool -> (AnnotationType, Expr TBool)
mkPost = (APost,)

mkInv :: Expr TBool -> (AnnotationType, Expr TBool)
mkInv = (AInv,)

data Vertex = Vertex
  { v_name :: Text
  , v_label :: Label
  , v_isOptimizing :: Bool
  }
  deriving (Show)

instance Eq Vertex where
  (==) lhs rhs = v_name lhs == v_name rhs

instance Ord Vertex where
  compare :: Vertex -> Vertex -> Ordering
  compare lhs rhs = v_name lhs `compare` v_name rhs

data ArcCondition = ACNone | ACJnz Label Bool
  deriving stock (Show)

data CFGBuildF a
  = AddVertex Label Bool (Vertex -> a)
  | AddArc Vertex Vertex [LabeledInst] ArcCondition FInfo a
  | AddAssertion Vertex (AnnotationType, Expr TBool) a
  | AskIdentifiers (Identifiers -> a)
  | -- | AskInstructions ([LabeledInst] -> a)
    AskProgram (Program -> a)
  | GetFuncSpec ScopedFunction (FuncSpec' -> a)
  | GetInvariant ScopedName (Maybe (Expr TBool) -> a)
  | GetRets ScopedName ([Label] -> a)
  | GetVerts Label ([Vertex] -> a)
  | Throw Text
  | forall b. Catch (CFGBuildL b) (Text -> CFGBuildL b) (b -> a)

deriving instance Functor CFGBuildF

newtype CFGBuildL a = CFGBuildL {runCFGBuildL :: F CFGBuildF a}
  deriving newtype (Functor, Applicative, Monad)

instance MonadError Text CFGBuildL where
  throwError = throw
  catchError = catch

liftF' :: CFGBuildF a -> CFGBuildL a
liftF' = CFGBuildL . liftF

addVertex :: Label -> CFGBuildL Vertex
addVertex l = liftF' (AddVertex l False id)

addOptimizingVertex :: Label -> CFGBuildL Vertex
addOptimizingVertex l = liftF' (AddVertex l True id)

addArc :: Vertex -> Vertex -> [LabeledInst] -> ArcCondition -> FInfo -> CFGBuildL ()
addArc vFrom vTo insts test f = liftF' (AddArc vFrom vTo insts test f ())

addAssertion :: Vertex -> (AnnotationType, Expr TBool) -> CFGBuildL ()
addAssertion v assertion = liftF' (AddAssertion v assertion ())

askIdentifiers :: CFGBuildL Identifiers
askIdentifiers = liftF' (AskIdentifiers id)

-- askInstructions :: CFGBuildL [LabeledInst]
-- askInstructions = liftF' (AskInstructions id)

askProgram :: CFGBuildL Program
askProgram = liftF' (AskProgram id)

getFuncSpec :: ScopedFunction -> CFGBuildL FuncSpec'
getFuncSpec name = liftF' (GetFuncSpec name id)

getInvariant :: ScopedName -> CFGBuildL (Maybe (Expr TBool))
getInvariant name = liftF' (GetInvariant name id)

getRets :: ScopedName -> CFGBuildL [Label]
getRets name = liftF' (GetRets name id)

getVerts :: Label -> CFGBuildL [Vertex]
getVerts l = liftF' (GetVerts l id)

-- It is enforced that for any one PC, one can add at most a single salient vertex
getSalientVertex :: Label -> CFGBuildL Vertex
getSalientVertex l = do
  verts <- filter (not . v_isOptimizing) <$> getVerts l
  -- traceM ("verts here: " ++ show verts)
  -- This can be at most one, so len <> 1 implies there are no vertices
  unless (length verts == 1) . throw $ "No vertex with label: " <> tShow l
  -- traceM "Survived."
  pure $ head verts

throw :: Text -> CFGBuildL a
throw t = liftF' (Throw t)

catch :: CFGBuildL a -> (Text -> CFGBuildL a) -> CFGBuildL a
catch m h = liftF' (Catch m h id)

buildCFG :: [LabeledInst] -> Set ScopedFunction -> CFGBuildL ()
buildCFG labeledInsts inlinable = do
  identifiers <- askIdentifiers
  prog <- askProgram
  buildFrame inlinable labeledInsts prog
  addAssertions inlinable identifiers

newtype Segment = Segment (NonEmpty LabeledInst)
  deriving (Show)

segmentLabel :: Segment -> Label
segmentLabel (Segment ((l, _) :| _)) = l

nextSegmentLabel :: Segment -> Label
nextSegmentLabel s = getNextPc (NonEmpty.last (coerce s))

segmentInsts :: Segment -> [LabeledInst]
segmentInsts (Segment ne) = toList ne

buildFrame :: Set ScopedFunction -> [LabeledInst] -> Program -> CFGBuildL ()
buildFrame inlinable rows prog = do
  let segments = breakIntoSegments (programLabels rows $ p_identifiers prog) rows
  segmentsWithVerts <- for segments $ \s -> addVertex (segmentLabel s) <&> (s,)
  for_ segmentsWithVerts $ \(s, v) -> addArcsFrom inlinable prog rows s v True
  -- let segments = breakIntoSegments (programLabels rows $ p_identifiers prog) rows
  -- vertices <- for segments $ \s -> addVertex (segmentLabel s) <&> (s,)
  -- -- let segmentsWithPrecedingVertices = zip segments (Nothing : map Just vertices)
  -- -- traceM ("segmentsWithPrecedingVertices: " ++ show segmentsWithPrecedingVertices)
  -- for_ vertices $ \(s, v) -> addArcsFrom inlinable prog rows s v True
  -- -- for_ segments $ \s -> do
  -- --   v <- addVertex (segmentLabel s)
  -- --   addArcsFrom inlinable prog rows s v True

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

addArc' :: Vertex -> Vertex -> [LabeledInst] -> CFGBuildL ()
addArc' lFrom lTo insts = addArc lFrom lTo insts ACNone Nothing

addArcsFrom :: Set ScopedFunction -> Program -> [LabeledInst] -> Segment -> Vertex -> Bool -> CFGBuildL ()
addArcsFrom inlinable prog rows s vFrom optimizeWithSplit
  | Call <- i_opCode endInst =
    let callee = uncheckedScopedFOfPc (p_identifiers prog) (uncheckedCallDestination lInst)
     in if callee `Set.member` inlinable
          then do
            salientCalleeV <- getSalientVertex (sf_pc callee)
            addArc vFrom salientCalleeV insts ACNone . Just $ ArcCall endPc (sf_pc callee)
          else do
            -- traceM ("calle here: " ++ show callee)
            salientLinearV <- getSalientVertex (nextSegmentLabel s)
            addArc' vFrom salientLinearV insts
            when optimizeWithSplit $ do
              ghostV <- addOptimizingVertex (nextSegmentLabel s)
              pre <- maybe (mkPre Expr.True) mkPre . fs'_pre <$> getFuncSpec callee
              addAssertion ghostV $ quantifyEx pre
              -- traceM ("original pre: " ++ show pre ++ " ex pre: " ++ show (quantifyEx pre))
              addArc' vFrom ghostV insts

              -- let ghostLabel = unspecifiedLabel $ segmentLabel s
              --     func = uncheckedScopedFOfPc (p_identifiers prog) calleePc in do
              

      -- -- let calleePc = uncheckedCallDestination lInst
      -- let callee = uncheckedScopedFOfPc (p_identifiers prog) (uncheckedCallDestination lInst)
      --  in do
      --   traceM ("vFrom: " ++ show mbVFrom ++ " segmentLabel: " ++ show s)
      --   when (callee `Set.notMember` inlinable && optimizeWithSplit) $ do
      --     -- Generate an extra 'dead-end' Vertex with an annotation containing the 
      --     -- pre of the function in question quantified by associated logical variables
      --     ghostVertex <- addVertex (segmentLabel s)
      --     pre <- maybe (mkPre Expr.True) mkPre . fs'_pre <$> getFuncSpec callee
      --     addAssertion ghostVertex $ quantifyEx pre
      --     traceM ("original pre: " ++ show pre ++ " ex pre: " ++ show (quantifyEx pre))
      --   -- One (1) or two (two) vertices, depending on inlinability / optimizeWithSplit
      --   calleeVs <- getVerts (segmentLabel s)
      --   traceM ("callee vs: " ++ show calleeVs)
      --   for_ calleeVs $ \v ->
      --     if callee `Set.member` inlinable
      --       then addArc vFrom v insts ACNone . Just . ArcCall endPc $ v_label v
      --       else addArc' vFrom v insts
  | Ret <- i_opCode endInst =
      let owner = sf_pc $ pcToFunOfProg prog ! endPc
       in do
        -- traceM "ret"
        -- traceM ("owner: " ++ show owner)
        endVertex <- getSalientVertex owner
        if owner `Set.notMember` inlinableLabels -- TODO: Don't think this ever triggers, try this.
            then pure ()
            else
              let callers = callersOf rows owner
               in do
                returnVs <- mapM (getSalientVertex . (`moveLabel` sizeOfCall)) callers
                forM_ returnVs $ \returnV -> addArc endVertex returnV [lInst] ACNone $ Just ArcRet
  | JumpAbs <- i_pcUpdate endInst = do
      -- traceM "jumpAbs"
      lTo <- getSalientVertex $ Label (fromInteger (i_imm endInst))
      addArc' vFrom lTo (init insts)
  | JumpRel <- i_pcUpdate endInst = do
      -- traceM "jump rel"
      lTo <- getSalientVertex $ moveLabel endPc (fromInteger (i_imm endInst))
      addArc' vFrom lTo (init insts)
  | Jnz <- i_pcUpdate endInst = do
      -- traceM "jnz"
      lTo1 <- getSalientVertex $ nextSegmentLabel s
      lTo2 <- getSalientVertex $ moveLabel endPc (fromInteger (i_imm endInst))
      addArc vFrom lTo1 insts (ACJnz endPc False) Nothing
      addArc vFrom lTo2 insts (ACJnz endPc True) Nothing
  | otherwise = do
      -- traceM "otherwise"
      -- vs <- getVerts $ nextSegmentLabel s
      -- traceM ("vfrom: " ++ show vFrom ++ " next: " ++ show vs ++ " nextSegLbl: " ++ show (nextSegmentLabel s))
      lTo <- getSalientVertex $ nextSegmentLabel s
      addArc' vFrom lTo insts
 where
  lInst@(endPc, endInst) = NonEmpty.last (coerce s)
  insts = segmentInsts s
  inlinableLabels = Set.map sf_pc inlinable

  quantifyEx :: (AnnotationType, Expr 'TBool) -> (AnnotationType, Expr 'TBool)
  quantifyEx = second $ \expr ->
    let lvars = gatherLogicalVariables expr in
    foldr Expr.ExistsFelt expr lvars

addAssertions :: Set ScopedFunction -> Identifiers -> CFGBuildL ()
addAssertions inlinable identifiers = do
  for_ (Map.toList identifiers) $ \(idName, def) -> case def of
    IFunction f ->
      let func = ScopedFunction idName (fu_pc f)
       in do
            pre <- fs'_pre <$> getFuncSpec func
            post <- fs'_post <$> getFuncSpec func
            retVs <- mapM getSalientVertex =<< getRets idName
            case (pre, post) of
              (Nothing, Nothing) ->
                when (fu_pc f `Set.notMember` Set.map sf_pc inlinable) $
                  for_ retVs (`addAssertion` mkPost Expr.True)
              _ -> for_ retVs (`addAssertion` maybe (mkPost Expr.True) mkPost post)
    ILabel pc ->
      whenJustM (getInvariant idName) $ \inv ->
        getSalientVertex pc >>= (`addAssertion` mkInv inv)
    _ -> pure ()
