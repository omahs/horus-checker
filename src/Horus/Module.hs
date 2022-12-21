{-# OPTIONS_GHC -Wno-unused-imports #-}
module Horus.Module
  ( Module (..)
  , ModuleL (..)
  , ModuleF (..)
  , Error (..)
  , gatherModules
  , nameOfModule
  , ModuleSpec (..)
  , PlainSpec (..)
  , richToPlainSpec
  )
where

import Control.Applicative ((<|>))
import Control.Monad (unless)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Free.Church (F, liftF)
import Data.Foldable (for_, traverse_)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Map qualified as Map (elems, empty, insert, null, toList)
import Data.Text (Text)
import Data.Text qualified as Text (concat, cons, intercalate, length)
import Lens.Micro (ix, (^.))
import Text.Printf (printf)

import Horus.CFGBuild (ArcCondition (..), Label (unLabel), Vertex (v_label), getVerts)
import Horus.CFGBuild.Runner (CFG (..))
import Horus.CallStack (CallStack, calledFOfCallEntry, callerPcOfCallEntry, initialWithFunc, pop, push, stackTrace, top)
import Horus.Expr (Expr, Ty (..), (.&&), (.==))
import Horus.Expr qualified as Expr (and)
import Horus.Expr.SMT (pprExpr)
import Horus.Expr.Vars (ap, fp)
import Horus.FunctionAnalysis (FInfo, FuncOp (ArcCall, ArcRet), isRetArc, sizeOfCall)
import Horus.Instruction (LabeledInst)
import Horus.Label (moveLabel)
import Horus.Program (Identifiers)
import Horus.SW.FuncSpec (FuncSpec (..))
import Horus.SW.Identifier (Function (..), getFunctionPc, getLabelPc)
import Horus.SW.ScopedName (ScopedName (..))
import Debug.Trace (traceM, trace)

data Module = Module
  { m_spec :: ModuleSpec
  , m_prog :: [LabeledInst]
  , m_jnzOracle :: Map (NonEmpty Label, Label) Bool
  , m_calledF :: Label
  , m_lastPc :: (CallStack, Label)
  }
  deriving stock (Show)

data ModuleSpec = MSRich FuncSpec | MSPlain PlainSpec
  deriving stock (Show)

data PlainSpec = PlainSpec {ps_pre :: Expr TBool, ps_post :: Expr TBool}
  deriving stock (Show)

richToPlainSpec :: FuncSpec -> PlainSpec
richToPlainSpec FuncSpec{..} = PlainSpec{ps_pre = fs_pre .&& ap .== fp, ps_post = fs_post}

beginOfModule :: [LabeledInst] -> Maybe Label
beginOfModule [] = Nothing
beginOfModule ((lbl, _) : _) = Just lbl

labelNamesOfPc :: Identifiers -> Label -> [ScopedName]
labelNamesOfPc idents lblpc =
  [ name
  | (name, ident) <- Map.toList idents
  , Just pc <- [getFunctionPc ident <|> getLabelPc ident]
  , pc == lblpc
  ]

normalizedName :: [ScopedName] -> (Text, Text)
normalizedName scopedNames =
  let names :: [[Text]]
      names = map sn_path scopedNames
      scopes = map (Text.intercalate "." . tail . init) names
      labels = map last names
   in (Text.concat scopes, summarizeLabels labels)
 where
  summarizeLabels labels =
    let prettyLabels = Text.intercalate "|" labels
     in if length labels == 1
          then prettyLabels
          else Text.concat ["{", prettyLabels, "}"]

descrOfBool :: Bool -> Text
descrOfBool True = "T"
descrOfBool False = "F"

descrOfOracle :: Map (NonEmpty Label, Label) Bool -> Text
descrOfOracle oracle =
  if Map.null oracle
    then ""
    else Text.cons '+' . Text.concat . map descrOfBool . Map.elems $ oracle

-- While we do have the name of the called function in Module, it does not contain
-- the rest of the labels.
nameOfModule :: Identifiers -> Module -> Text
nameOfModule idents (Module spec prog oracle _ _) =
  case beginOfModule prog of
    Nothing -> "empty: " <> pprExpr post
    Just label ->
      let (prefix, labelsDigest) = normalizedName $ labelNamesOfPc idents label
          noPrefix = Text.length prefix == 0
       in Text.concat [prefix, if noPrefix then "" else ".", labelsDigest, descrOfOracle oracle]
 where
  post = case spec of MSRich fs -> fs_post fs; MSPlain ps -> ps_post ps

data Error
  = ELoopNoInvariant Label
  | ESpecNotPlainHasState

instance Show Error where
  show (ELoopNoInvariant at) = printf "There is a loop at PC %d with no invariant" (unLabel at)
  show ESpecNotPlainHasState =
    "Some function contains a loop, but uses rich specfication (e.g. state assertions)."

data ModuleF a
  = EmitModule Module a
  | forall b. Visiting (NonEmpty Label, Label) (Bool -> ModuleL b) (b -> a)
  | Throw Error
  | forall b. Catch (ModuleL b) (Error -> ModuleL b) (b -> a)

deriving stock instance Functor ModuleF

newtype ModuleL a = ModuleL {runModuleL :: F ModuleF a}
  deriving newtype (Functor, Applicative, Monad)

instance MonadError Error ModuleL where
  throwError = throw
  catchError = catch

liftF' :: ModuleF a -> ModuleL a
liftF' = ModuleL . liftF

-- | Emit the module 'm', which needs to be verified.
emitModule :: Module -> ModuleL ()
emitModule m = liftF' (EmitModule m ())

{- | Perform the action on the path where the label 'l' has been marked
   as visited.

'm' additionally takes a parameter that tells whether 'l' has been
visited before.
-}
visiting :: (NonEmpty Label, Label) -> (Bool -> ModuleL b) -> ModuleL b
visiting l action = liftF' (Visiting l action id)

throw :: Error -> ModuleL a
throw t = liftF' (Throw t)

catch :: ModuleL a -> (Error -> ModuleL a) -> ModuleL a
catch m h = liftF' (Catch m h id)

data SpecBuilder = SBRich | SBPlain (Expr TBool)

extractPlainBuilder :: FuncSpec -> ModuleL SpecBuilder
extractPlainBuilder fs@(FuncSpec _pre _post state)
  | not (null state) = throwError ESpecNotPlainHasState
  | PlainSpec{..} <- richToPlainSpec fs = pure (SBPlain ps_pre)

gatherModules :: CFG -> [(Function, ScopedName, FuncSpec)] -> ModuleL ()
gatherModules cfg = traverse_ $ \(f, _, spec) -> gatherFromSource cfg f spec

gatherFromSource :: CFG -> Function -> FuncSpec -> ModuleL ()
gatherFromSource cfg function fSpec = do
  verticesAt <- liftF . getVerts $ fu_pc function
  for_ verticesAt $ \v ->
    visit Map.empty (initialWithFunc (fu_pc function)) [] SBRich v ACNone Nothing
 where
  visit ::
    Map (NonEmpty Label, Label) Bool ->
    CallStack ->
    [LabeledInst] ->
    SpecBuilder ->
    Vertex ->
    ArcCondition ->
    FInfo ->
    ModuleL ()
  visit oracle callstack acc builder v arcCond f =
    visiting (stackTrace callstack', l) $ \alreadyVisited ->
      if alreadyVisited then visitLoop builder else visitLinear builder
   where
    l = v_label v

    visitLoop SBRich = extractPlainBuilder fSpec >>= visitLoop
    visitLoop (SBPlain pre)
      | null assertions = throwError (ELoopNoInvariant l)
      | otherwise = emitPlain pre (Expr.and assertions)

    visitLinear SBRich
      | onFinalNode = emitRich (fs_pre fSpec) (Expr.and $ map snd (cfg_assertions cfg ^. ix v))
      | null assertions = visitArcs oracle' acc builder v
      | otherwise = extractPlainBuilder fSpec >>= visitLinear
    visitLinear (SBPlain pre)
      | null assertions = visitArcs oracle' acc builder v
      | otherwise = do
          emitPlain pre (Expr.and assertions)
          visitArcs Map.empty [] (SBPlain (Expr.and assertions)) v

    callstack' = case f of
      Nothing -> callstack
      Just (ArcCall fCallerPc fCalledF) -> push (fCallerPc, fCalledF) callstack
      Just ArcRet -> snd $ pop callstack
    oracle' = updateOracle arcCond callstack' oracle
    assertions = trace ("querying at: " ++ show l ++ " for " ++ show (cfg_assertions cfg ^. ix v)) $ map snd (cfg_assertions cfg ^. ix v)
    onFinalNode = null (cfg_arcs cfg ^. ix v)
    emitPlain pre post = emitModule (Module (MSPlain (PlainSpec pre post)) acc oracle' (calledFOfCallEntry $ top callstack') (callstack', l))
    emitRich pre post = emitModule (Module (MSRich (FuncSpec pre post (fs_storage fSpec))) acc oracle' (calledFOfCallEntry $ top callstack') (callstack', l))

    visitArcs newOracle acc' pre v' = do
      let outArcs = cfg_arcs cfg ^. ix v'
      unless (null outArcs) $
        let isCalledBy = (moveLabel (callerPcOfCallEntry $ top callstack') sizeOfCall ==)
            outArcs' = filter (\(dst, _, _, f') -> not (isRetArc f') || isCalledBy dst) outArcs
         in for_ outArcs' $ \(lTo, insts, test, f') ->
              visit newOracle callstack' (acc' <> insts) pre lTo test f'

updateOracle ::
  ArcCondition ->
  CallStack ->
  Map (NonEmpty Label, Label) Bool ->
  Map (NonEmpty Label, Label) Bool
updateOracle ACNone _ = id
updateOracle (ACJnz jnzPc isSat) callstack =
  Map.insert (stackTrace callstack, jnzPc) isSat
