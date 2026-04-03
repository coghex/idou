{-# LANGUAGE Strict, UnicodeSyntax #-}

module Main where

import Control.Exception (SomeException, try)
import Control.Monad (forM, unless, when)
import Data.List (intercalate, isInfixOf)
import System.Environment (getArgs)
import System.Exit (exitFailure)

import TestSupport (TestCase(..))
import qualified Tests.Audio as Audio
import qualified Tests.ConfigPatch as ConfigPatch
import qualified Tests.Timeline as Timeline

main ∷ IO ()
main = do
  filters <- getArgs
  let selected = filter (matchesAny filters . tcName) testCases
  when (null selected) $ do
    putStrLn ("No tests matched filters: " <> unwords filters)
    putStrLn ("Available tests: " <> intercalate ", " (map tcName testCases))
    exitFailure

  results <- forM selected runTest
  let failures = [name | (name, False) <- results]
  unless (null failures) $ do
    putStrLn ("Failed tests: " <> intercalate ", " failures)
    exitFailure

matchesAny ∷ [String] → String → Bool
matchesAny [] _ = True
matchesAny needles haystack = any (`isInfixOf` haystack) needles

runTest ∷ TestCase → IO (String, Bool)
runTest tc = do
  putStrLn ("[test] " <> tcName tc)
  result <- try (tcAction tc) ∷ IO (Either SomeException ())
  case result of
    Left ex -> do
      putStrLn ("[fail] " <> tcName tc <> ": " <> show ex)
      pure (tcName tc, False)
    Right () -> do
      putStrLn ("[pass] " <> tcName tc)
      pure (tcName tc, True)

testCases ∷ [TestCase]
testCases =
  ConfigPatch.testCases
    <> Timeline.testCases
    <> Audio.testCases
