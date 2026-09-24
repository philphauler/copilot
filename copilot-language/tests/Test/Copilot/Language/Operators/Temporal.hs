-- | Test copilot-language:Copilot.Language.Operators.Temporal.
--
-- The tests build streams by prepending elements to other streams and then
-- dropping elements from them, and compare the result of evaluating those
-- streams against the same operations applied to their expected meaning.
module Test.Copilot.Language.Operators.Temporal where

-- External imports
import Data.Int                             (Int8)
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck                      (Arbitrary, Gen, Property,
                                             arbitrary, chooseInt, forAll,
                                             forAllShow, oneof, vectorOf)
import Test.QuickCheck.Monadic              (monadicIO, run)

-- Internal imports: library modules being tested
import           Copilot.Language                    (Typed)
import qualified Copilot.Language.Operators.Temporal as Copilot
import           Copilot.Language.Stream             (Stream)

-- Internal imports: auxiliary functions
import Test.Copilot.Language.Reify (SemanticsP (..), arbitraryBoolExpr,
                                    arbitraryNumExpr, checkSemanticsP,
                                    maxTraceLength, semanticsShowK)

-- | All unit tests for copilot-language:Copilot.Language.Operators.Temporal.
tests :: Test.Framework.Test
tests =
  testGroup "Copilot.Language.Operators.Temporal"
    [ testProperty "drop up to the number of elements prepended"
        testDropPrepended
    ]

-- * Individual tests

-- | Test that dropping any number of elements up to the number of elements
-- prepended to a stream, possibly split over several drops and with the
-- elements prepended in several steps, produces the expected values.
testDropPrepended :: Property
testDropPrepended =
  forAll (chooseInt (0, maxTraceLength)) $ \steps ->
  forAllShow arbitraryDropSemanticsP (semanticsShowK steps) $ \pair ->
  monadicIO $ run (checkSemanticsP steps [] pair)

-- * Random generators

-- | An arbitrary stream built with prepends and drops, paired with its
-- expected meaning.
arbitraryDropSemanticsP :: Gen SemanticsP
arbitraryDropSemanticsP = oneof
  [ SemanticsP <$> arbitraryDrop boolExpr
  , SemanticsP <$> arbitraryDrop int8Expr
  ]
  where
    boolExpr :: Gen (Stream Bool, [Bool])
    boolExpr = arbitraryBoolExpr

    int8Expr :: Gen (Stream Int8, [Int8])
    int8Expr = arbitraryNumExpr

-- | Prepend elements to an arbitrary stream in one or more steps, and then
-- drop up to the total number of elements prepended in one or more steps.
arbitraryDrop :: (Arbitrary t, Typed t)
              => Gen (Stream t, [t])
              -> Gen (Stream t, [t])
arbitraryDrop gen = do
  (stream, meaning) <- gen

  -- Prepend elements to the stream, in one to three steps.
  numPrepends <- chooseInt (1, 3)
  prepends    <- vectorOf numPrepends $ do
                   len <- chooseInt (1, 4)
                   vectorOf len arbitrary

  let prepended        = foldr (Copilot.++) stream prepends
      prependedMeaning = concat prepends ++ meaning
      numPrepended     = sum (map length prepends)

  -- Drop up to the total number of elements prepended, in one to three steps.
  numDrops <- chooseInt (0, numPrepended)
  drops    <- splitInto numDrops

  let dropped        = foldr Copilot.drop prepended drops
      droppedMeaning = drop numDrops prependedMeaning

  return (dropped, droppedMeaning)

-- | Split a number into one to three non-negative numbers that add up to it.
splitInto :: Int -> Gen [Int]
splitInto n = do
  numParts <- chooseInt (1, 3)
  go numParts n
  where
    go :: Int -> Int -> Gen [Int]
    go 1 m = return [m]
    go k m = do
      x    <- chooseInt (0, m)
      rest <- go (k - 1) (m - x)
      return (x : rest)
