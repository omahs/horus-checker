module Horus.Preprocessor.Solvers (Solver (..), cvc5, yices, mathsat, z3) where

import Data.Text as Text (Text, drop, init, pack, tail, unpack)
import qualified SimpleSMT as SMT
  ( Result (..)
  , SExpr (..)
  , check
  , command
  , loadString
  , newSolver
  , ppSExpr
  , stop
  )

newtype Solver = Solver {runSolver :: Text -> IO (SMT.Result, Maybe Text)}

cvc5 :: Solver
cvc5 =
  Solver $
    fmap (fmap . fmap $ Text.tail . Text.init)
      . sideSolver "cvc5" ["--produce-models"]

yices :: Solver
yices = Solver $ sideSolver "yices" []

mathsat :: Solver
mathsat =
  Solver $
    fmap (fmap . fmap $ Text.drop 6 . Text.init) -- extracting ... from (model ...)
      . sideSolver "mathsat" ["-model_generation=True"]

z3 :: Solver
z3 =
  Solver $
    fmap (fmap . fmap $ Text.tail . Text.init)
      . sideSolver "z3" ["-in", "-model"]

sideSolver :: String -> [String] -> Text -> IO (SMT.Result, Maybe Text)
sideSolver solverName args sexpr = do
  solver <- SMT.newSolver solverName args Nothing
  SMT.loadString solver (unpack sexpr)
  res <- SMT.check solver
  mbModelOrReason <- case res of
    SMT.Sat -> do
      model <- SMT.command solver (SMT.List [SMT.Atom "get-model"])
      return $ Just $ SMT.ppSExpr model ""
    SMT.Unknown -> do
      reason <- SMT.command solver (SMT.List [SMT.Atom "get-info", SMT.Atom ":reason-unknown"])
      return $ Just $ SMT.ppSExpr reason ""
    SMT.Unsat -> return Nothing
  let output = pack <$> mbModelOrReason
  _ <- SMT.stop solver
  return (res, output)
