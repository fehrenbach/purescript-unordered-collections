module Bench.Main where

import Prelude

import Control.Monad.Eff.Console (log)
import Data.Array (range)
import Data.Foldable (foldr)
import Data.HashMap (HashMap)
import Data.HashMap as HM
import Data.Hashable (class Hashable)
import Data.List (List)
import Data.Map (Map)
import Data.Map as OM
import Data.Tuple (Tuple(..))
import Performance.Minibench (bench)

si :: Int -> Array (Tuple String Int)
si n = map (\i -> Tuple (show i) i) $ range 1 n

is :: Int -> Array (Tuple Int String)
is n = map (\i -> Tuple i (show i)) $ range 1 n

insertHM :: forall k v. Hashable k => Array (Tuple k v) -> HashMap k v
insertHM a = HM.fromFoldable a

insertOM :: forall k v. Ord k => Array (Tuple k v) -> Map k v
insertOM a = OM.fromFoldable a

main = do
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

  log "HM.toUnfoldableUnordered"
  bench \_ -> HM.toUnfoldableUnordered hmSi100 :: Array _

  -- log "HM.toArrayUnordered"
  -- bench \_ -> HM.toArrayUnordered hmSi100

  log "OM.toUnfoldable"
  bench \_ -> OM.toUnfoldable omSi100 :: Array _

  log "HM.values (List)"
  bench \_ -> HM.values hmSi100 :: List Int

  log "OM.values (List)"
  bench \_ -> OM.values omSi100 :: List Int

  log "HM.values (Array)"
  bench \_ -> HM.values hmSi100 :: Array Int
