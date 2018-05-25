-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Bench.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array (range)
import Data.Array as Array
import Data.Foldable (foldr)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.HashMap (HashMap)
import Data.HashMap as HM
import Data.Hashable (class Hashable)
import Data.List (List)
import Data.Map (Map)
import Data.Map as OM
import Data.Tuple (Tuple(..))
import Performance.Minibench (bench, benchWith)

si :: Int -> Array (Tuple String Int)
si n = map (\i -> Tuple (show i) i) $ range 1 n

is :: Int -> Array (Tuple Int String)
is n = map (\i -> Tuple i (show i)) $ range 1 n

insertHM :: forall k v. Hashable k => Array (Tuple k v) -> HashMap k v
insertHM a = HM.fromFoldable a

insertOM :: forall k v. Ord k => Array (Tuple k v) -> Map k v
insertOM a = OM.fromFoldable a

main :: forall t34. Eff ( console :: CONSOLE | t34 ) Unit
main = do
  let is10000 = is 10000
  let iKeys10000 = range 1 10000
  let hmIs10000 = insertHM is10000
  let omIs10000 = insertOM is10000

  log "HM singleton"
  benchWith 1000000 \_ -> HM.singleton 5 42

  log "OM singleton"
  benchWith 1000000 \_ -> OM.singleton 5 42

  log "HM insert 10000 distinct integers"
  bench \_ -> insertHM is10000

  log "OM insert 10000 distinct integers"
  bench \_ -> insertOM is10000


  log "HM lookup all 10000 distinct integers"
  bench \_ -> map (\i -> HM.lookup i hmIs10000) iKeys10000

  log "OM lookup all 10000 distinct integers"
  bench \_ -> map (\i -> OM.lookup i omIs10000) iKeys10000

  let is100 = is 100
  let hmIs100 = insertHM is100
  let omIs100 = insertOM is100
  let iKeys100 = range 1 100

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
