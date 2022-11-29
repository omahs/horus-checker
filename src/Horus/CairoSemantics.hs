{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Horus.CairoSemantics
  ( encodeModule
  , CairoSemanticsF (..)
  , CairoSemanticsL
  , BuiltinOffsets (..)
  , MemoryVariable (..)
  )
where

import Control.Monad (forM_, unless, when)
import Control.Monad.Free.Church (F, liftF)
import Data.Foldable (for_, toList, traverse_)
import Data.Functor ((<&>))
import Data.List (tails)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty (head, last, tail, toList)
import Data.Map (Map)
import Data.Map qualified as Map ((!?))
import Data.Set qualified as Set (member)
import Data.Text (Text)

import Horus.CallStack as CS (CallEntry, callerOfRoot, CallStack)
import Horus.Expr (Cast (..), Expr ((:+)), Ty (..), (.&&), (./=), (.<), (.<=), (.==), (.=>), (.||))
import Horus.Expr qualified as Expr
import Horus.Expr.Util (gatherLogicalVariables, suffixLogicalVariables)
import Horus.Expr.Vars
  ( builtinAligned
  , builtinConstraint
  , builtinEnd
  , builtinStart
  , memory
  , prime
  , regToVar
  , pattern Memory
  , pattern StorageVar
  )
import Horus.Instruction
  ( ApUpdate (..)
  , Instruction (..)
  , LabeledInst
  , Op1Source (..)
  , OpCode (..)
  , ResLogic (..)
  , callDestination
  , getNextPc
  , isCall
  , isRet
  , uncheckedCallDestination, getNextPcInlined, getNextPcInlinedWithFallback
  )
import Horus.Label (Label (..), tShowLabel)
import Horus.Module (Module (..), ModuleSpec (..), PlainSpec (..), richToPlainSpec)
import Horus.Program (ApTracking (..))
import Horus.SW.Builtin (Builtin, BuiltinOffsets (..))
import Horus.SW.Builtin qualified as Builtin (name)
import Horus.SW.FuncSpec (FuncSpec (..), FuncSpec', toFuncSpec)
import Horus.SW.ScopedName (ScopedName (ScopedName))
import Horus.SW.ScopedName qualified as ScopedName (fromText)
import Horus.SW.Storage (Storage)
import Horus.SW.Storage qualified as Storage (equivalenceExpr)
import Horus.Util (enumerate, tShow, whenJust, whenJustM)
import Horus.Expr.Vars qualified as Util
import Horus.Expr qualified as TSMT
import Debug.Trace (trace, traceM)
import Horus.Expr.Vars qualified as Vars

data MemoryVariable = MemoryVariable
  { mv_varName :: Text
  , mv_addrName :: Text
  , mv_addrExpr :: Expr TFelt
  }
  deriving (Show)

data CairoSemanticsF a
  = Assert' (Expr TBool) a
  | Expect' (Expr TBool) a
  | CheckPoint ([MemoryVariable] -> Expr TBool) a
  | DeclareMem (Expr TFelt) (Expr TFelt -> a)
  | DeclareLocalMem (Expr TFelt) (MemoryVariable -> a)
  | GetCallee LabeledInst (ScopedName -> a)
  | GetFuncSpec ScopedName (FuncSpec' -> a)
  | GetApTracking Label (ApTracking -> a)
  | GetFunPc Label (Label -> a)
  | GetBuiltinOffsets Label Builtin (Maybe BuiltinOffsets -> a)
  | GetStackTraceDescr (Maybe CallStack) (Text -> a)
  | GetOracle (NonEmpty Label -> a)
  | IsInlinable Label (Bool -> a)
  | Push CallEntry a
  | Pop a
  | Top (CallEntry -> a)
  | EnableStorage a
  | ReadStorage ScopedName [Expr TFelt] (Expr TFelt -> a)
  | UpdateStorage Storage a
  | GetStorage (Storage -> a)
  | Throw Text
  deriving stock (Functor)

type CairoSemanticsL = F CairoSemanticsF

assert' :: Expr TBool -> CairoSemanticsL ()
assert' a = liftF (Assert' a ())

expect' :: Expr TBool -> CairoSemanticsL ()
expect' a = liftF (Expect' a ())

checkPoint :: ([MemoryVariable] -> Expr TBool) -> CairoSemanticsL ()
checkPoint a = liftF (CheckPoint a ())

declareMem :: Expr TFelt -> CairoSemanticsL (Expr TFelt)
declareMem address = liftF (DeclareMem address id)

getCallee :: LabeledInst -> CairoSemanticsL ScopedName
getCallee call = liftF (GetCallee call id)

getFuncSpec :: ScopedName -> CairoSemanticsL FuncSpec'
getFuncSpec name = liftF (GetFuncSpec name id)

declareLocalMem :: Expr TFelt -> CairoSemanticsL MemoryVariable
declareLocalMem address = liftF (DeclareLocalMem address id)

getApTracking :: Label -> CairoSemanticsL ApTracking
getApTracking inst = liftF (GetApTracking inst id)

getFunPc :: Label -> CairoSemanticsL Label
getFunPc l = liftF (GetFunPc l id)

getBuiltinOffsets :: Label -> Builtin -> CairoSemanticsL (Maybe BuiltinOffsets)
getBuiltinOffsets l b = liftF (GetBuiltinOffsets l b id)

throw :: Text -> CairoSemanticsL a
throw t = liftF (Throw t)

enableStorage :: CairoSemanticsL ()
enableStorage = liftF (EnableStorage ())

readStorage :: ScopedName -> [Expr TFelt] -> CairoSemanticsL (Expr TFelt)
readStorage name args = liftF (ReadStorage name args id)

updateStorage :: Storage -> CairoSemanticsL ()
updateStorage storage = liftF (UpdateStorage storage ())

getStorage :: CairoSemanticsL Storage
getStorage = liftF (GetStorage id)

assert :: Expr TBool -> CairoSemanticsL ()
assert a = assert' =<< memoryRemoval a

expect :: Expr TBool -> CairoSemanticsL ()
expect a = expect' =<< memoryRemoval a

memoryRemoval :: Expr a -> CairoSemanticsL (Expr a)
memoryRemoval = Expr.transform step
 where
  step :: Expr b -> CairoSemanticsL (Expr b)
  step (Memory x) = declareMem x
  step e = pure e

isInlinable :: Label -> CairoSemanticsL Bool
isInlinable l = liftF (IsInlinable l id)

getStackTraceDescr' :: Maybe CallStack -> CairoSemanticsL Text
getStackTraceDescr' callstack = liftF (GetStackTraceDescr callstack id)

getStackTraceDescr :: CairoSemanticsL Text
getStackTraceDescr = getStackTraceDescr' Nothing

getOracle :: CairoSemanticsL (NonEmpty Label)
getOracle = liftF (GetOracle id)

push :: CallEntry -> CairoSemanticsL ()
push l = liftF (Push l ())

pop :: CairoSemanticsL ()
pop = liftF (Pop ())

top :: CairoSemanticsL CallEntry
top = liftF (Top id)

isCtxRoot :: CairoSemanticsL Bool
isCtxRoot = (callerOfRoot ==) . fst <$> top

storageRemoval :: Expr a -> CairoSemanticsL (Expr a)
storageRemoval mexp = let res = Expr.transform step mexp in
  do
  r <- res
  -- traceM ("called storageRemovel on: " ++ show mexp ++ " resulting in: " ++ show r)
  res
 where
  step :: Expr b -> CairoSemanticsL (Expr b)
  step (StorageVar name args) = readStorage (ScopedName.fromText name) args
  step e = pure e

substitute :: Text -> Expr TFelt -> Expr a -> Expr a
substitute what forWhat = Expr.canonicalize . Expr.transformId step
 where
  step :: Expr b -> Expr b
  step (Expr.cast @TFelt -> CastOk (Expr.Fun name)) | name == what = forWhat
  step e = e

{- | Prepare the expression for usage in the model.

That is, deduce AP from the ApTracking data by PC and replace FP name
with the given one.
-}
prepare :: Label -> Expr TFelt -> Expr a -> CairoSemanticsL (Expr a)
prepare pc fp expr = getAp pc >>= \ap -> prepare' ap fp expr

prepare' :: Expr TFelt -> Expr TFelt -> Expr a -> CairoSemanticsL (Expr a)
prepare' ap fp expr = memoryRemoval (substitute "fp" fp (substitute "ap" ap expr))

prepareCheckPoint ::
  Label -> Expr TFelt -> Expr TBool -> CairoSemanticsL ([MemoryVariable] -> Expr TBool)
prepareCheckPoint pc fp expr = do
  ap <- getAp pc
  exMemoryRemoval (substitute "fp" fp (substitute "ap" ap expr))

encodeApTracking :: Text -> ApTracking -> Expr TFelt
encodeApTracking traceDescr ApTracking{..} =
  Expr.const ("ap!" <> traceDescr <> "@" <> tShow at_group) + fromIntegral at_offset

getAp' :: Maybe CallStack -> Label -> CairoSemanticsL (Expr TFelt)
getAp' callstack pc = getStackTraceDescr' callstack >>= \stackTrace -> getApTracking pc <&> encodeApTracking stackTrace

getFp' :: Maybe CallStack -> CairoSemanticsL (Expr TFelt)
getFp' callstack = getStackTraceDescr' callstack <&> Expr.const . ("fp!" <>)

getAp :: Label -> CairoSemanticsL (Expr TFelt)
getAp = getAp' Nothing

getFp :: CairoSemanticsL (Expr TFelt)
getFp = getFp' Nothing

moduleStartAp :: Module -> CairoSemanticsL (Expr TFelt)
moduleStartAp Module{m_prog = []} = getStackTraceDescr <&> Expr.const . ("ap!" <>)
moduleStartAp Module{m_prog = (pc0, _) : _} = getAp pc0

moduleEndAp :: Module -> CairoSemanticsL (Expr TFelt)
moduleEndAp Module{m_prog = []} = getStackTraceDescr <&> Expr.const . ("ap!" <>)
moduleEndAp m@Module{m_prog = _} = getAp' (Just callstack) pc where (callstack, pc) = m_lastPc m

encodeModule :: Module -> CairoSemanticsL ()
encodeModule m@Module{..} = case m_spec of
  MSRich spec -> encodeRichSpec m spec
  MSPlain spec -> encodePlainSpec m spec

encodeRichSpec :: Module -> FuncSpec -> CairoSemanticsL ()
encodeRichSpec mdl funcSpec@(FuncSpec _pre _post storage) = do
  enableStorage
  fp <- getFp
  apEnd <- moduleEndAp mdl
  -- traceM ("apEnd in rich: " ++ show apEnd)
  preparedStorage <- traverseStorage (prepare' apEnd fp) storage
  -- traceM ("Prepared storage for module: " ++ show (m_calledF mdl) ++ " is: " ++ show preparedStorage)
  encodePlainSpec mdl plainSpec
  accumulatedStorage <- getStorage
  -- traceM ("Accumulated storage for module: " ++ show (m_calledF mdl) ++ " is: " ++ show accumulatedStorage)
  expect (Storage.equivalenceExpr accumulatedStorage preparedStorage)
 where
  plainSpec = richToPlainSpec funcSpec

encodePlainSpec :: Module -> PlainSpec -> CairoSemanticsL ()
encodePlainSpec mdl PlainSpec{..} = do
  apStart <- moduleStartAp mdl
  apEnd <- moduleEndAp mdl
  -- traceM ("module last PC: " ++ show (m_lastPc mdl))
  -- traceM ("apEnd in plain: " ++ show apEnd)
  fp <- getFp
  assert (fp .<= apStart)
  pre <- prepare' apStart fp ps_pre
  post <- prepare' apEnd fp ps_post
  assert pre
  let instrs = zip (m_prog mdl) [0..]
  for_ instrs $ \(instr, pos) -> mkInstructionConstraints (m_prog mdl) pos (m_jnzOracle mdl) instr
  expect post
  traceM ("new module" ++ show (m_calledF mdl))
  whenJust (nonEmpty (m_prog mdl)) $ \neInsts -> do
    mkApConstraints apEnd neInsts
    mkBuiltinConstraints apEnd neInsts

exMemoryRemoval :: Expr TBool -> CairoSemanticsL ([MemoryVariable] -> Expr TBool)
exMemoryRemoval expr = do
  (expr', localMemVars, _referencesLocals) <- unsafeMemoryRemoval expr
  pure (intro expr' localMemVars)
 where
  exVars = gatherLogicalVariables expr

  restrictMemTail [] = []
  restrictMemTail (mv0 : rest) =
    [ addr0 .== Expr.const mv_addrName .=> mem0 .== Expr.const mv_varName
    | MemoryVariable{..} <- rest
    ]
   where
    mem0 = Expr.const (mv_varName mv0)
    addr0 = Expr.const (mv_addrName mv0)

  intro :: Expr TBool -> [MemoryVariable] -> [MemoryVariable] -> Expr TBool
  intro ex lmv gmv =
    let globMemRestrictions =
          [ addr1 .== addr2 .=> Expr.const var1 .== Expr.const var2
          | MemoryVariable var1 _ addr1 <- lmv
          , MemoryVariable var2 _ addr2 <- gmv
          ]
        locMemRestrictions = concatMap restrictMemTail (tails lmv)
        innerExpr = Expr.and (ex : (locMemRestrictions ++ globMemRestrictions))
        quantLmv = foldr (\mvar e -> Expr.ExistsFelt (mv_varName mvar) e) innerExpr lmv
     in foldr (\var e -> Expr.ExistsFelt var e) quantLmv exVars

  unsafeMemoryRemoval :: Expr a -> CairoSemanticsL (Expr a, [MemoryVariable], Bool)
  unsafeMemoryRemoval (Memory addr) = do
    (addr', localMemVars, referencesLocals) <- unsafeMemoryRemoval addr
    if referencesLocals
      then do
        mv <- declareLocalMem addr'
        pure (Expr.const (mv_varName mv), mv : localMemVars, True)
      else do
        mv <- declareMem addr'
        pure (mv, localMemVars, False)
  unsafeMemoryRemoval e@Expr.Felt{} = pure (e, [], False)
  unsafeMemoryRemoval e@Expr.True = pure (e, [], False)
  unsafeMemoryRemoval e@Expr.False = pure (e, [], False)
  unsafeMemoryRemoval e@(Expr.Fun name) = pure (e, [], name `Set.member` exVars)
  unsafeMemoryRemoval (f Expr.:*: x) = do
    (f', localMemVars1, referencesLocals1) <- unsafeMemoryRemoval f
    (x', localMemVars2, referencesLocals2) <- unsafeMemoryRemoval x
    pure (f' Expr.:*: x', localMemVars2 <> localMemVars1, referencesLocals1 || referencesLocals2)
  unsafeMemoryRemoval (Expr.ExistsFelt name e) = do
    (e', localMemVars, referencesLocals) <- unsafeMemoryRemoval e
    pure (Expr.ExistsFelt name e', localMemVars, referencesLocals)
  unsafeMemoryRemoval (Expr.ExitField e) = do
    (e', localMemVars, referencesLocals) <- unsafeMemoryRemoval e
    pure (Expr.ExitField e', localMemVars, referencesLocals)

withExecutionCtx :: CallEntry -> CairoSemanticsL b -> CairoSemanticsL b
withExecutionCtx ctx action = do
  push ctx
  res <- action
  pop
  pure res

mkInstructionConstraints :: [LabeledInst] -> Int -> Map (NonEmpty Label, Label) Bool -> LabeledInst -> CairoSemanticsL ()
mkInstructionConstraints instrs pos jnzOracle lInst@(pc, Instruction{..}) = do
  -- traceM ("current instr pc: " ++ show pc ++ " with imm: " ++ show i_imm)
  fp <- getFp
  dst <- prepare pc fp (memory (regToVar i_dstRegister + fromInteger i_dstOffset))
  case i_opCode of
    Call -> mkCallConstraints pc pos instrs fp lInst
    AssertEqual -> getRes fp lInst >>= \res -> assert (res .== dst)
    Nop -> do
      stackTrace <- getOracle
      case jnzOracle Map.!? (stackTrace, pc) of
        Just False -> assert (dst .== 0)
        Just True -> assert (dst ./= 0)
        Nothing -> pure ()
    Ret ->
      -- trace (show ("found ret at: " ++ show pc))
      pop

traceWhen :: Bool -> String -> CairoSemanticsL ()
traceWhen isPertinent x = do when isPertinent $ traceM x

mkCallConstraints :: Label -> Int -> [LabeledInst] -> Expr TFelt -> LabeledInst -> CairoSemanticsL ()
mkCallConstraints pc pos instrs fp inst =
  let calleePc = uncheckedCallDestination inst
      -- nextPc' = getNextPc inst
      nextPc = getNextPcInlinedWithFallback instrs pos
      stackFrame = (pc, calleePc)
   in do
        -- traceM ("instrs are: " ++ show instrs)
        -- traceM ("IN CALL with current instr: " ++ show inst ++ " nextPc': " ++ show nextPc' ++ " nextPc: " ++ show nextPc)
        dbgCallee <- getCallee inst
        let isRead = dbgCallee == ScopedName ["__main__", "balance", "read"]
        let traceWhenRead = traceWhen isRead
        -- traceM ("In call:" ++ show dbgCallee)
        calleeFp <- withExecutionCtx stackFrame getFp
        nextAp <- prepare pc calleeFp (Vars.fp .== Vars.ap + 2)
        saveOldFp <- prepare pc fp (memory Vars.ap .== Vars.fp)
        -- traceM ("current instr pc: " ++ show inst)
        -- traceM ("unLabel nextPc: " ++ show (unLabel nextPc))
        setNextPc <- prepare pc fp (memory (Vars.ap + 1) .== fromIntegral (unLabel nextPc))
        assert (TSMT.and [nextAp, saveOldFp, setNextPc])
        push stackFrame
        canInline <- isInlinable calleePc
        -- traceWhenRead ("can inline: " ++ show calleePc ++ "? " ++ show canInline)
        unless canInline $ do
          (FuncSpec pre post storage) <- (getCallee inst >>= getFuncSpec) <&> toFuncSpec
          -- traceWhenRead ("function: " ++ show calleePc ++ " abstracting with storage: " ++ show storage)
          let pre' = suffixLogicalVariables lvarSuffix pre
              post' = suffixLogicalVariables lvarSuffix post
          removedStorage <- storageRemoval pre'
          -- traceWhenRead ("removed storage: " ++ show removedStorage)
          preparedPre <- prepare calleePc calleeFp removedStorage
          -- traceWhenRead ("prepared pre: " ++ show preparedPre)
          preparedPreCheckPoint <- prepareCheckPoint calleePc calleeFp =<< storageRemoval pre'
          -- traceWhenRead ("updating storage in: " ++ show calleeFp)
          dbgStrg <- traverseStorage (prepare nextPc calleeFp) storage
          -- traceWhenRead ("new storage: " ++ show dbgStrg)
          updateStorage dbgStrg
          pop
          rm1 <- storageRemoval post'
          rm2 <- storageRemoval rm1
          preparedPost <- prepare nextPc calleeFp rm2
          -- traceWhenRead ("removed post': " ++ show rm1 ++ " \nremoved again: " ++ show rm2 ++ " \npost: " ++ show post ++ " -> " ++ show preparedPost)
          -- -- preparedPost <- prepare nextPc calleeFp =<< storageRemoval =<< storageRemoval post'
          -- traceWhenRead ("prepared post: " ++ show preparedPost)
          checkPoint preparedPreCheckPoint
          assert (preparedPre .&& preparedPost)
  where lvarSuffix = "+" <> tShowLabel pc

traverseStorage :: (forall a. Expr a -> CairoSemanticsL (Expr a)) -> Storage -> CairoSemanticsL Storage
traverseStorage preparer = traverse prepareWrites
 where
  prepareWrites = traverse prepareWrite
  prepareWrite (args, value) = (,) <$> traverse prepareExpr args <*> prepareExpr value
  prepareExpr e = storageRemoval e >>= preparer

mkApConstraints :: Expr TFelt -> NonEmpty LabeledInst -> CairoSemanticsL ()
mkApConstraints apEnd insts = do
  forM_ (zip (toList insts) (NonEmpty.tail insts)) $ \(lInst@(pc, inst), (pcNext, _)) -> do
    at1 <- getApTracking pc
    at2 <- getApTracking pcNext
    traceM ("pc: " ++ show pc ++ " pcNext: " ++ show pcNext ++ " at1: " ++ show at1 ++ " at2: " ++ show at2)
    canInline <- isInlinable $ uncheckedCallDestination lInst
    when (at_group at1 /= at_group at2) $ do
      ap1 <- getAp pc
      if isCall inst && canInline
        then push (pc, uncheckedCallDestination lInst)
        else when (isRet inst) pop
      ap2 <- getAp pcNext
      fp <- getFp
      getApIncrement fp lInst >>= \case
        Just apIncrement -> let res = assert (ap1 + apIncrement .== ap2) in
          trace ("en passant -- generating the eq : " ++ show (ap1 + apIncrement .== ap2)) $ res
          -- res
        Nothing | not canInline -> trace "was call and can't inline: " $ assert (ap1 .< ap2)
        Nothing -> trace "was call and can inline" $ pure ()
  lastAp <- getAp lastPc
  -- traceM ("last ap is: " ++ show lastAp)
  when (isRet lastInst) $ do 
    -- traceM "the last instruction is ret" 
    pop
  fp <- getFp
  -- traceM ("current FP: " ++ show fp)
  getApIncrement fp lastLInst >>= \case
    Just lastApIncrement -> let res = assert (lastAp + lastApIncrement .== apEnd) in
      trace ("last -- generating the eq: " ++ show (lastAp + lastApIncrement .== apEnd)) $ res
      -- res
    Nothing -> assert (lastAp .< apEnd)
 where
  lastLInst@(lastPc, lastInst) = NonEmpty.last insts

mkBuiltinConstraints :: Expr TFelt -> NonEmpty LabeledInst -> CairoSemanticsL ()
mkBuiltinConstraints apEnd insts = do
  -- traceM ("making builtin constraints")
  fp <- getFp
  -- traceM ("making builtin constraints, apEnd: " ++ show apEnd ++ " fp: " ++ show fp)
  funPc <- getFunPc (fst (NonEmpty.head insts))
  for_ enumerate $ \b ->
    getBuiltinOffsets funPc b >>= \case
      Just bo -> do
        let (pre, post) = getBuiltinContract fp apEnd b bo
        assert pre *> expect post
        -- TODO: Hic Sunt Dracones
        -- traceM ("-------> INSTS ARE: " ++ show insts)
        for_ (zip (NonEmpty.toList insts) [0..]) $ \(inst, i) -> mkBuiltinConstraintsForInst apEnd (i == length insts - 1) i (NonEmpty.toList insts) b inst
      Nothing -> checkBuiltinNotRequired b (toList insts)

getBuiltinContract ::
  Expr TFelt -> Expr TFelt -> Builtin -> BuiltinOffsets -> (Expr TBool, Expr TBool)
getBuiltinContract fp apEnd b bo = (pre, post)
 where
  pre = builtinAligned initialPtr b .&& finalPtr .<= builtinEnd b
  post = initialPtr .<= finalPtr .&& builtinAligned finalPtr b
  initialPtr = memory (fp - fromIntegral (bo_input bo))
  finalPtr = memory (apEnd - fromIntegral (bo_output bo))

mkBuiltinConstraintsForInst :: Expr TFelt -> Bool -> Int -> [LabeledInst] -> Builtin -> LabeledInst -> CairoSemanticsL ()
mkBuiltinConstraintsForInst apEnd isLast pos instrs b inst@(pc, Instruction{..}) =
  -- trace ("current inst: " ++ show inst ++ " current apEnd: " ++ show apEnd) $
  -- trace ("CALLED mkBuiltinCFI - apEnd: " ++ show apEnd ++ " isLast: " ++ show isLast ++ "pc: " ++ show pc) $
  trace ("making builtins for: " ++ show b)
  getFp >>= \fp -> do
    stackTrace <- getStackTraceDescr
    -- traceM ("STACK TRACE: " ++ show stackTrace)
    case i_opCode of
      Call ->
        let calleePc = uncheckedCallDestination inst
            stackFrame = (pc, calleePc)
         in do
              -- traceM ("mkBuiltin - push: " ++ show calleePc)
              push stackFrame
              traceM ("PUSH!: " ++ show stackFrame)
              canInline <- isInlinable calleePc
              unless canInline $ do
                -- traceM "this is NOT inlinable call"
                mkBuiltinConstraintsForFunc apEnd False
                t <- top
                traceM ("POP!: " ++ show t)
                -- pop
      AssertEqual -> do
        res <- getRes fp inst
        case res of
          Memory resAddr -> assert (builtinConstraint resAddr b)
          op0 :+ op1 -> do
            let isBuiltin = builtinStart b .<= op0 .|| builtinStart b .<= op1
            assert (isBuiltin .=> Expr.ExitField (op0 + op1 .== (op0 + op1) `Expr.mod` prime))
          _ -> pure ()
      -- 'ret's are not in the bytecote for functions that are not inlinable
      Ret -> do
        mkBuiltinConstraintsForFunc apEnd True
        t <- top
        traceM ("POP!: " ++ show t)
        -- pop
      _ -> pure ()
 where
  mkBuiltinConstraintsForFunc lastAp canInline = do
    -- traceM (">>>>>> forFun with lastAp: " ++ show lastAp)
    calleeFp <- getFp
    callEntry@(_, calleePc) <- 
      do
        -- traceM "mkBuiltin - pop"
        top <* pop
    stackTrace <- getStackTraceDescr
    isRoot <- isCtxRoot 
    traceM ("STACK TRACE: " ++ show stackTrace ++ " is root: " ++ show isRoot)
    traceM ("getting builtin for b: " ++ show b ++ " at pc: " ++ show calleePc)
    gg <- getBuiltinOffsets calleePc b
    traceM ("got builtins offsets: " ++ show gg)
    -- unless canInline pop
    -- pop
    -- NOTE TO SELF: add_two should never be seen by this! (skip over the push?)
    whenJustM (getBuiltinOffsets calleePc b) $ \bo -> do
      traceM ("isLast: " ++ show isLast)
      traceM ("canInline: " ++ show canInline)
      traceM ("current instruction: " ++ show inst)
      -- TODO: NOTE TO SELF - IF THIS IS FALSE, IT'S SAT PROPER
      -- when canInline pop
      resr <- getAp (getNextPcInlinedWithFallback instrs pos)
      traceM ("nextAp: " ++ show resr)
      traceM ("next last PC: " ++ show (getNextPcInlinedWithFallback instrs pos))
      traceM ("lastAP: " ++ show lastAp ++ " getNextAP: " ++ show resr)

      -- calleeApEnd <- if canInline then withExecutionCtx callEntry (getAp (fst $ instrs !! (pos - 1))) else getAp (getNextPcInlinedWithFallback instrs pos)

      calleeApEnd <- if canInline then withExecutionCtx callEntry (getAp pc) else getAp (getNextPcInlinedWithFallback instrs pos)

      -- calleeApEnd <- if canInline then pure lastAp else getAp (getNextPcInlinedWithFallback instrs pos)

      -- calleeApEnd <- if not isLast
      --   -- ret
      --   -- then getAp (getNextPcInlinedWithFallback instrs pos)
      --   then getAp (getNextPcInlinedWithFallback instrs pos)
      --   -- call
      --   else pure lastAp

          -- call
          -- pure lastAp
      -- calleeApEnd <- if isLast then pure lastAp else if canInline then getAp pc else getAp (getNextPcInlinedWithFallback instrs pos)
      -- calleeApEnd <- getAp (getNextPcInlinedWithFallback instrs pos)
      -- calleeApEnd <-
      --   if not canInline -- e || not isLast -- && not isRoot?
      --     then do
      --       res <- getAp (getNextPcInlinedWithFallback instrs pos)
      --       -- traceM ("nextPc': " ++ show (getNextPcInlinedWithFallback instrs pos) ++ " nextPc: " ++ show (getNextPc inst))
      --       traceM ("[[standard last ap: " ++ show res ++ "]]")
      --       -- traceM ("res: " ++ show res ++ " lastAp: " ++ show lastAp)
      --       getAp (getNextPcInlinedWithFallback instrs pos)
      --     else do
      --       traceM ("[[custom last ap: " ++ show lastAp ++ "]]")
      --       pure lastAp
      -- traceM ("callee AP end: " ++ show calleeApEnd)
      traceM ("getting contract for calleeFp: " ++ show calleeFp ++ " calleeApEnd: " ++ show calleeApEnd)
      let (pre, post) = getBuiltinContract calleeFp calleeApEnd b bo
      -- traceM ("expecting PRE: " ++ show pre)
      traceM ("asserting post: " ++ show post ++ " expecting pre: " ++ show pre)
      expect pre *> assert post
    -- pop

checkBuiltinNotRequired :: Builtin -> [LabeledInst] -> CairoSemanticsL ()
checkBuiltinNotRequired b = traverse_ check
 where
  check inst = whenJust (callDestination inst) $ \calleePc ->
    whenJustM (getBuiltinOffsets calleePc b) $ \_ ->
      throw
        ( "The function doesn't require the '"
            <> Builtin.name b
            <> "' builtin, but calls a function (at PC "
            <> tShow (unLabel calleePc)
            <> ") that does require it"
        )

getRes :: Expr TFelt -> LabeledInst -> CairoSemanticsL (Expr TFelt)
getRes fp (pc, Instruction{..}) = do
  op0Reg <- prepare pc fp (regToVar i_op0Register)
  let op0 = memory (op0Reg + fromInteger i_op0Offset)
  op1 <- case i_op1Source of
    Op0 -> pure (memory (op0 + fromInteger i_op1Offset))
    RegisterSource reg -> do
      op1Reg <- prepare pc fp (regToVar reg)
      pure (memory (op1Reg + fromInteger i_op1Offset))
    Imm -> pure (fromInteger i_imm)
  pure $ case i_resLogic of
    Op1 -> op1
    Add -> op0 + op1
    Mult -> op0 * op1
    Unconstrained -> 0

getApIncrement :: Expr TFelt -> LabeledInst -> CairoSemanticsL (Maybe (Expr TFelt))
getApIncrement fp inst
  | Call <- i_opCode (snd inst) = pure Nothing
  | otherwise = fmap Just $ case i_apUpdate (snd inst) of
      NoUpdate -> pure 0
      Add1 -> pure 1
      Add2 -> pure 2
      AddRes -> getRes fp inst
