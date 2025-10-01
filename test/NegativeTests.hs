{-# LANGUAGE GHC2021, LambdaCase, DataKinds #-}

-- Compile the module with type errors, allowing to assert they throw at test runtime
{-# OPTIONS_GHC -fdefer-type-errors #-}

module NegativeTests (checkCompileRejections) where

import Control.Exception as Exception
import qualified Data.Monoid as Monoid
import Data.Proxy

import Fcf.Core
import Fcf.Combinators
import Fcf.Class.Functor (Map)
import Fcf.Data.List (Length)
import Fcf.Utils

-- | A helper to force evaluation of type families to normal form.
--
-- It has several instances, thus instance resolution must case-match on the instance head,
-- causing Eval to compute.
class ForceType a where
  forceType :: Proxy a -> ()
  forceType = const ()
instance {-# OVERLAPS #-} ForceType ()
instance {-# OVERLAPPING #-} ForceType anytype

evalType :: forall {k} (tyexpr :: Exp k) (r :: k). (r ~ Eval tyexpr, ForceType r) => IO ()
evalType = case forceType @r Proxy of {() -> pure ()}

assertYieldsTypeError :: forall {k} (tyexpr :: Exp k) (r :: k). (r ~ Eval tyexpr, ForceType r) => IO ()
assertYieldsTypeError = try @Exception.TypeError (evalType @tyexpr) >>= \case
  Right _ -> throw $ AssertionFailed "Test compiled, but TypeError was expected"
  Left (TypeError _) -> pure () -- assert passed, raised TypeError as expected

assertAbsentTypeError :: forall {k} (tyexpr :: Exp k) (r :: k). (r ~ Eval tyexpr, ForceType r) => IO ()
assertAbsentTypeError = try @Exception.TypeError (evalType @tyexpr) >>= \case
  Right _ -> pure () -- assert passed, no TypeError
  Left (TypeError e) -> throw $
    AssertionFailed ("Example raised compile error, but was expected to compile:\n\n" <> e)

checkCompileRejections :: IO ()
checkCompileRejections = do
  assertAbsentTypeError @(Pure () >>= Assert ('Text "thrown") (Pure True))
  assertYieldsTypeError @(Pure Int >>= Assert ('Text "thrown") (Pure False))

  assertAbsentTypeError @(AssertNot ('Text "thrown") (Pure False) =<< Map (Pure1 Maybe) '[Bool, Int])
  -- assertYieldsTypeError @(AssertNot ('Text "thrown") (Pure True) =<< Map (Pure1 Maybe) '[Bool, Int])
