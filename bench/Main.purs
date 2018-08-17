-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Bench.Main where

import Prelude

import Data.Array (range)
import Data.Array as Array
import Data.Foldable (foldMap, foldl, foldr)
import Data.FoldableWithIndex (foldMapWithIndex, foldrWithIndex)
import Data.HashMap (HashMap)
import Data.HashMap as HM
import Data.HashSet as HS
import Data.Hashable (class Hashable)
import Data.List (List)
import Data.Map (Map)
import Data.Map as OM
import Data.Maybe (Maybe(..))
import Data.Set as OS
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Performance.Minibench (bench, benchWith)

si :: Int -> Array (Tuple String Int)
si n = map (\i -> Tuple (show i) i) $ range 1 n

is :: Int -> Array (Tuple Int String)
is n = map (\i -> Tuple i (show i)) $ range 1 n

insertHM :: forall k v. Hashable k => Array (Tuple k v) -> HashMap k v
insertHM a = HM.fromFoldable a

insertOM :: forall k v. Ord k => Array (Tuple k v) -> Map k v
insertOM a = OM.fromFoldable a

main :: Effect Unit
main = do
  let is10000 = is 10000
  let is20000 = is 10000 <> is 10000
  let iKeys10000 = range 1 10000
  let hmIs10000 = insertHM is10000
  let omIs10000 = insertOM is10000

  let is100 = is 100
  let hmIs100 = insertHM is100
  let omIs100 = insertOM is100
  let iKeys100 = range 1 100

  log "HM singleton"
  benchWith 1000000 \_ -> HM.singleton 5 42

  log "OM singleton"
  benchWith 1000000 \_ -> OM.singleton 5 42

  log "HM insert 10000 distinct integers"
  bench \_ -> insertHM is10000

  log "OM insert 10000 distinct integers"
  bench \_ -> insertOM is10000

  log "HM insertWith (<>) (2x 10000 integers)"
  bench \_ -> foldr (\(Tuple i x) -> HM.insertWith (<>) i x) HM.empty is20000

  log "HM lookup all 10000 distinct integers"
  bench \_ -> map (\i -> HM.lookup i hmIs10000) iKeys10000

  log "OM lookup all 10000 distinct integers"
  bench \_ -> map (\i -> OM.lookup i omIs10000) iKeys10000

  log "HM insert 100 distinct integers"
  bench \_ -> insertHM is100

  log "OM insert 100 distinct integers"
  bench \_ -> insertOM is100

  log "HM lookup all 100 distinct integers"
  bench \_ -> map (\i -> HM.lookup i hmIs100) iKeys100

  log "OM lookup all 100 distinct integers"
  bench \_ -> map (\i -> OM.lookup i omIs100) iKeys100

  log "HM delete all 100 distinct keys"
  bench \_ -> foldr (\i m -> HM.delete i m) hmIs100 iKeys100

  log "OM delete all 100 distinct keys"
  bench \_ -> foldr (\i m -> OM.delete i m) omIs100 iKeys100

  log ""
  log "Bulk insert"
  log "-----------"

  log "HM foldl is10000"
  bench \_ -> foldl (\m (Tuple k v) -> HM.insert k v m) HM.empty is10000

  log "HM fromFoldable is10000"
  bench \_ -> HM.fromFoldable is10000

  log "HM foldMap is10000"
  bench \_ -> foldMap (\(Tuple k v) -> HM.singleton k v) is10000

  log ""
  log "STRING KEYS"
  log "-----------"
  let si100 = si 100
  let hmSi100 = insertHM si100
  let omSi100 = insertOM si100
  let sKeys100 = map show (range 1 100)

  log "HM insert 100 distinct strings"
  bench \_ -> insertHM si100

  log "OM insert 100 distinct strings"
  bench \_ -> insertOM si100

  log "HM lookup all 100 distinct integers"
  bench \_ -> map (\i -> HM.lookup i hmSi100) sKeys100

  log "OM lookup all 100 distinct integers"
  bench \_ -> map (\i -> OM.lookup i omSi100) sKeys100

  log "HM delete all 100 distinct keys"
  bench \_ -> foldr (\i m -> HM.delete i m) hmSi100 sKeys100

  log "OM delete all 100 distinct keys"
  bench \_ -> foldr (\i m -> OM.delete i m) omSi100 sKeys100

  log ""
  log "Unfoldable etc."
  log "---------------"

  log "OM.toUnfoldable (List)"
  bench \_ -> OM.toUnfoldable omSi100 :: List (Tuple String Int)

  log "OM.toUnfoldable (Array)"
  bench \_ -> OM.toUnfoldable omSi100 :: Array (Tuple String Int)

  log "HM.toArrayBy Tuple"
  bench \_ -> HM.toArrayBy Tuple hmSi100

  log "HM to array using foldMapWithIndex"
  bench \_ -> foldMapWithIndex (\k v -> Array.singleton (Tuple k v)) hmSi100

  log "OM.values (List)"
  bench \_ -> OM.values omSi100 :: List Int

  log "HM.values (Array)"
  bench \_ -> HM.values hmSi100 :: Array Int

  -- log "OM.values (Array)"
  -- bench \_ -> OM.values omSi100 :: List Int

  log "HM.keys (Array)"
  bench \_ -> HM.keys hmSi100 :: Array String

  -- log "OM.keys (Array)"
  -- bench \_ -> OM.keys omSi100 :: Array String

  log ""
  log "Foldable Traversable"
  log "--------------------"

  log "HM map Just and sequence 10000"
  bench \_ -> sequence $ Just <$> hmIs10000

  log "OM map Just and sequence 10000"
  bench \_ -> sequence $ Just <$> omIs10000

  log ""
  log "UnionWith"
  log "---------"

  log "HM union"
  bench \_ -> HM.union hmIs10000 hmIs10000

  log "HM unionWith const"
  bench \_ -> HM.unionWith const hmIs10000 hmIs10000

  log "HM repeated insertion"
  bench \_ -> foldrWithIndex HM.insert hmIs10000 hmIs10000

  log "HM difference 100 10000"
  bench \_ -> HM.difference hmIs100 hmIs10000

  -- log "OM difference 100 10000"
  -- bench \_ -> OM.difference omIs100 omIs10000

  log "HM difference 10000 100"
  bench \_ -> HM.difference hmIs10000 hmIs100

  -- log "OM difference 10000 100"
  -- bench \_ -> OM.difference omIs10000 omIs100

  log ""
  log "Nub"
  log "---"

  let ints10 = range 1 10
  log "Array.nub 10 ints"
  bench \_ -> Array.nub ints10

  log "nubHash 10 ints"
  bench \_ -> HM.nubHash ints10

  let ints20 = range 1 10 <> range 1 10
  log "Array.nub 20 ints"
  bench \_ -> Array.nub ints20

  log "nubHash 20 ints"
  bench \_ -> HM.nubHash ints20

  let ints1000 = range 1 1000
  log "Array.nub 1000 ints"
  bench \_ -> Array.nub ints1000

  log "nubHash 1000 ints"
  bench \_ -> HM.nubHash ints1000

  let ints2000 = range 1 1000 <> range 1 1000
  log "Array.nub 2000 ints"
  bench \_ -> Array.nub ints2000

  log "nubHash 2000 ints"
  bench \_ -> HM.nubHash ints2000

  let strings1000 = map show $ range 1 1000
  log "Array.nub 1000 strings"
  bench \_ -> Array.nub strings1000

  log "nubHash 1000 strings"
  bench \_ -> HM.nubHash strings1000

  log "toArray <<< HashSet.fromFoldable 1000 strings"
  bench \_ -> HS.toArray (HS.fromFoldable strings1000)

  let strings2000 = strings1000 <> strings1000
  log "Array.nub 2000 strings"
  bench \_ -> Array.nub strings2000

  log "nubHash 2000 strings"
  bench \_ -> HM.nubHash strings2000

  log "toArray <<< HashSet.fromFoldable 2000 strings"
  bench \_ -> HS.toArray (HS.fromFoldable strings2000)

  log ""
  log "Sets"
  log "-----------"

  let i100 = range 0 100
  let os100 = OS.fromFoldable i100
  let hs100 = HS.fromFoldable i100

  let i10000 = range 0 10000
  let os10000 = OS.fromFoldable i10000
  let hs10000 = HS.fromFoldable i10000

  log "HS.union i100 i10000"
  bench \_ -> hs100 `HS.union` hs10000

  log "OS.union i100 i10000"
  bench \_ -> os100 `OS.union` os10000

  log "HS.union i10000 i100 "
  bench \_ -> hs10000 `HS.union` hs100

  log "OS.union i10000 i100"
  bench \_ -> os10000 `OS.union` os100

  log "HS.intersection i100 i10000"
  bench \_ -> hs100 `HS.intersection` hs10000

  log "OS.intersection i100 i10000"
  bench \_ -> os100 `OS.intersection` os10000

  log "HS.intersection i10000 i100 "
  bench \_ -> hs10000 `HS.intersection` hs100

  log "OS.intersection i10000 i100"
  bench \_ -> os10000 `OS.intersection` os100

  log "HS.difference i100 i10000"
  bench \_ -> hs100 `HS.difference` hs10000

  log "OS.difference i100 i10000"
  bench \_ -> os100 `OS.difference` os10000

  log "HS.difference i10000 i100 "
  bench \_ -> hs10000 `HS.difference` hs100

  log "OS.difference i10000 i100"
  bench \_ -> os10000 `OS.difference` os100
