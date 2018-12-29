-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Bench.Main where

import Prelude

import Control.Monad.ST (foreach, run)
import Data.Array as Array
import Data.FoldableWithIndex (foldrWithIndex)
import Data.HashMap as HM
import Data.HashSet as HS
import Data.Int (ceil, even, toNumber)
import Data.Map as OM
import Data.Maybe (Maybe(..))
import Data.Set as OS
import Data.Traversable (for)
import Effect (Effect)
import Effect.Class.Console (log)
import Foreign.Object (Object)
import Foreign.Object.ST (new, poke)
import Foreign.Object.ST.Unsafe (unsafeFreeze)
import Performance.Minibench (benchWith', withUnits)
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (Gen, randomSample')
import Unsafe.Coerce (unsafeCoerce)

foreign import data BF :: Type -> Type

type BenchFun a = { name :: String, fun :: BF a }

benchFun :: forall a b. String -> (a -> Unit -> b) -> BenchFun a
benchFun name fun = { name, fun: unsafeCoerce fun }

unBF :: forall a b. BF a -> (a -> Unit -> b)
unBF = unsafeCoerce

type Benchmark a = { description :: String
                   , sizes :: Array Int
                   , input :: Int -> Effect a
                   , implementations :: Array (BenchFun a) }

foreign import table :: forall a. a -> Effect Unit

guesstimateRuns :: forall a. Int -> (Unit -> a) -> Effect Int
guesstimateRuns patience f = go 1
  where
  precision = 100
  goalNS = toNumber patience * 1e9 / toNumber precision
  go :: Int -> Effect Int
  go runs = do
    { mean } <- benchWith' runs f
    if (mean * toNumber runs >= goalNS)
      then pure (runs * precision)
      else go (ceil (goalNS / mean))

runBenchmark :: forall a. Int -> Benchmark a -> Effect Unit
runBenchmark patience { description, input, sizes, implementations } = do
  log description
  outer <- for sizes \size -> do
    -- log $ "generate input at size " <> show size
    i <- input size
    inner <- for implementations \{name, fun} -> do
      let f = unsafeCoerce fun i
      -- log $ "estimate runtime for " <> name
      runs <- guesstimateRuns patience f
      -- log $ "run " <> show runs <> " iterations of " <> name
      { mean, min, stdDev, max } <- benchWith' runs f
      pure [{ size, name: name, mean, min, stdDev, max, runs }]
    pure (join inner)
  let t = map (\ { size, name, mean, min, stdDev, max, runs } ->
                { size, name,
                  min: withUnits min,
                  mean: withUnits mean,
                  max: withUnits max,
                  stdDev: withUnits stdDev,
                  runs })
          (join outer)
  -- log description
  table t

foreign import shuffle :: forall a. Array a -> Effect (Array a)

randomNumbers :: Int -> Effect (Array Number)
randomNumbers size = randomSample' size arbitrary

randomCoordinates :: Int -> Effect (Array { x :: Number, y :: Number })
randomCoordinates size = do xs <- randomNumbers size
                            ys <- randomNumbers size
                            pure (Array.zipWith (\x y -> {x, y}) xs ys)

foreign import bulkLoadStringKeysObj :: Array Int -> Object Int

main :: Effect Unit
main = do
  runBenchmark 10 { description: "bulk loading from random order Array (Tuple Int String)"
                  , sizes: [ 100, 10000 ]
                  , input: \s -> do
                    keys <- randomSample' s (arbitrary :: Gen Int)
                    values <- randomSample' s (arbitrary :: Gen String)
                    pure (Array.zip keys values)
                  , implementations: [ benchFun "HM.fromArray" (\i _ -> HM.fromArray i)
                                     , benchFun "HM.fromFoldable" (\i _ -> HM.fromFoldable i)
                                     -- , benchFun "HM.fromFoldableBy" (\i _ -> HM.fromFoldableBy fst snd i)
                                     -- , benchFun "HM.foldMap" (\i _ -> foldMap (\(Tuple k v) -> HM.singleton k v) i)
                                     , benchFun "OM.fromFoldable" (\i _ -> OM.fromFoldable i) ] }

  runBenchmark 10 { description: "look up all integer keys in random order"
                  , sizes: [ 100, 10000 ]
                  , input: \s -> do
                    keys <- randomSample' s (arbitrary :: Gen Int)
                    values <- randomSample' s (arbitrary :: Gen String)
                    let a = Array.zip keys values
                    pure { keys, hm: HM.fromFoldable a, om: OM.fromFoldable a }
                  , implementations: [ benchFun "HM.lookup" (\{hm, keys} _ -> map (\k -> HM.lookup k hm) keys)
                                     , benchFun "OM.lookup" (\{om, keys} _ -> map (\k -> OM.lookup k om) keys) ] }

  runBenchmark 10 { description: "delete all integer keys in random order"
                  , sizes: [ 100, 10000 ]
                  , input: \s -> do
                    keys <- randomSample' s (arbitrary :: Gen Int)
                    values <- randomSample' s (arbitrary :: Gen String)
                    let a = Array.zip keys values
                    pure { keys, hm: HM.fromFoldable a, om: OM.fromFoldable a }
                  , implementations: [ benchFun "HM.delete" (\{hm, keys} _ -> map (\k -> HM.delete k hm) keys)
                                     , benchFun "OM.delete" (\{om, keys} _ -> map (\k -> OM.delete k om) keys) ] }

  runBenchmark 10 { description: "filter random map Int Int"
                  , sizes: [ 100, 10000 ]
                  , input: \s -> do
                    keys <- randomSample' s arbitrary
                    values <- randomSample' s arbitrary
                    let a = Array.zip keys values
                    pure { hm: HM.fromArray a
                         , om: OM.fromFoldable a }
                  , implementations: [ benchFun "HM.filterWithKey both   even" \{hm} _ -> HM.filterWithKey (\k v -> even k && even v) hm
                                     , benchFun "HM.filterWithKey either even" \{hm} _ -> HM.filterWithKey (\k v -> even k || even v) hm
                                     , benchFun "OM.filterWithKey both   even" \{om} _ -> OM.filterWithKey (\k v -> even k && even v) om
                                     , benchFun "OM.filterWithKey either even" \{om} _ -> OM.filterWithKey (\k v -> even k || even v) om
                                     ] }

  let hmMapMaybeWithKeyFoldrWIInsert f = foldrWithIndex (\k v m -> case f k v of
                                                            Nothing -> m
                                                            Just w -> HM.insert k w m) HM.empty
  runBenchmark 10 { description: "mapMaybeWithKey random map Int Int"
                  , sizes: [ 100, 10000 ]
                  , input: \s -> do
                    keys <- randomSample' s arbitrary
                    values <- randomSample' s arbitrary
                    let a = Array.zip keys values
                    pure { hm: HM.fromArray a
                         , om: OM.fromFoldable a }
                  , implementations: [ benchFun "HM.mapMaybeWithKey" \{hm} _ ->
                                        HM.mapMaybeWithKey (\k v -> if even k then Just (k+v) else Nothing) hm
                                     , benchFun "HM foldrWI+insert" \{hm} _ ->
                                        hmMapMaybeWithKeyFoldrWIInsert (\k v -> if even k then Just (k+v) else Nothing) hm
                                     , benchFun "OM.mapMaybeWithKey" \{om} _ ->
                                        OM.mapMaybeWithKey (\k v -> if even k then Just (k+v) else Nothing) om
                                     ] }

  runBenchmark 10 { description: "set operations on coordinates (half of the elements are shared)"
                  , sizes: [ 100, 10000 ]
                  , input: \s -> do
                    left <- randomCoordinates (s / 2)
                    right <- randomCoordinates (s / 2)
                    common <- randomCoordinates (s / 2)
                    let la = left <> common
                    let ra = right <> common
                    pure { lhs: HS.fromFoldable la
                         , rhs: HS.fromFoldable ra
                         , los: OS.fromFoldable la
                         , ros: OS.fromFoldable ra }
                  , implementations: [ benchFun "HS.union" (\{lhs, rhs} _ -> HS.union lhs rhs)
                                     , benchFun "HS.difference" (\{lhs, rhs} _ -> HS.difference lhs rhs)
                                     , benchFun "HS.intersection" (\{lhs, rhs} _ -> HS.intersection lhs rhs)
                                     , benchFun "OS.union" (\{los, ros} _ -> OS.union los ros)
                                     , benchFun "OS.difference" (\{los, ros} _ -> OS.difference los ros)
                                     , benchFun "OS.intersection" (\{los, ros} _ -> OS.intersection los ros)
                                     ]
                  }

  runBenchmark 10 { description: "nub on an array of coordinates (~50% duplicates)"
                  , sizes: [ 100, 10000 ]
                  , input: \s -> do
                    uniques <- randomCoordinates (s / 2)
                    duplicates <- randomCoordinates (s / 4)
                    shuffle (uniques <> duplicates <> duplicates)
                  , implementations: [ benchFun "HM.nubHash" (\a _ -> HM.nubHash a)
                                     , benchFun "Array.nub" (\a _ -> Array.nub a)
                                     -- , benchFun "Array.nubEq" (\a _ -> Array.nubEq a)
                                     ]
                  }
  runBenchmark 10 { description: "bulk load string keys, numbers*10, range 1 1000000"
                  , sizes: [ 1000000 ]
                  , input: \s -> pure (Array.range 0 s)
                  , implementations: [ benchFun "HM.fromArrayBy" (\i _ -> HM.fromArrayBy show (_ * 10) i)
                                     , benchFun "FFI JavaScript" (\i _ -> bulkLoadStringKeysObj i)
                                     , benchFun "STForeignObject" (\arr _ -> run do
                                                         m <- new
                                                         _ <- foreach arr (\i -> do
                                                                              _ <- poke (show i) (i * 10) m
                                                                              pure unit)
                                                         unsafeFreeze m)
                                     ]
                  }
