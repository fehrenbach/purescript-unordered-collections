-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Test.Main where

import Prelude

import Data.Array as A
import Data.Array as Array
import Data.Foldable (foldMap, foldr)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.HashMap as HashMap
import Data.Hashable (class Hashable, hash)
import Data.List (List(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (alaF)
import Data.NonEmpty ((:|))
import Data.Traversable (sequence, traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Test.QuickCheck (Result(..), quickCheck, quickCheck', (<?>), (===))
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)

data Op k v
  = Lookup k
  | Insert k v

instance arbOp :: (Arbitrary k, Arbitrary v) => Arbitrary (Op k v) where
  arbitrary = oneOf $ (Insert <$> arbitrary <*> arbitrary) :| [Lookup <$> arbitrary]

newtype CollidingInt = CollidingInt Int

derive instance eqCollidingInt :: Eq CollidingInt
derive instance ordCollidingInt :: Ord CollidingInt

instance showCollidingInt :: Show CollidingInt where
  show (CollidingInt i) = show i

instance arbitraryCollidingInt :: Arbitrary CollidingInt where
  arbitrary = CollidingInt <$> arbitrary

instance collidingIntHashable :: Hashable CollidingInt where
  hash (CollidingInt i) = i `mod` 100

prop ::
  forall k v.
  Eq v => Show v =>
  Show k => Ord k => Hashable k =>
  List (Op k v) -> Result
prop = go Map.empty HashMap.empty
  where go m hm Nil = Success
        go m hm (Cons (Lookup k) rest) =
          let res = Map.lookup k m === HashMap.lookup k hm
          in case res of
            Success -> go m hm rest
            _ -> res
        go m hm (Cons (Insert k v) rest) =
          go (Map.insert k v m) (HashMap.insert k v hm) rest

arbitraryHashMap :: forall k v. Hashable k => Array (Tuple k v) -> HashMap.HashMap k v
arbitraryHashMap = HashMap.fromFoldable

nowGood :: forall a. Eq a => a -> a -> Effect Unit
nowGood a b = if a == b then log "Fixed \\o/" else throw "still broken"

nowGood' :: Boolean -> Effect Unit
nowGood' b = if b then log "Fixed \\o/" else throw "still broken"

main :: Effect Unit
main = do
  if 155 == hash {a: 5}
    then log "Hashable record passed"
    else throw "Hashable record failed"

  if 2987129 == hash {a: 5, b: false, c: "abc"}
    then log "Hashable record 2 passed"
    else throw "Hashable record 2 failed"

  log "Insert & lookup like map"
  quickCheck (prop :: List (Op Boolean Int) -> Result)
  quickCheck (prop :: List (Op Int Int) -> Result)
  quickCheck (prop :: List (Op CollidingInt CollidingInt) -> Result)

  log "Insert and lookup"
  quickCheck' 10000 $ \(k :: Int) (v :: Int) a ->
    HashMap.lookup k (HashMap.insert k v (arbitraryHashMap a)) == Just v
    <?> ("k: " <> show k <> ", v: " <> show v <> ", m: " <> show a <> ", r: " <> show (HashMap.lookup k (HashMap.insert k v (arbitraryHashMap a))))

  log "toArrayBy"
  quickCheck' 10000 $ \ (a :: Array (Tuple CollidingInt Int)) ->
    let nubA = A.nubBy (\x y -> fst x `compare` fst y) a
        m = arbitraryHashMap nubA
    in A.sort (HashMap.toArrayBy Tuple m) == A.sort nubA
       <?> ("expected: " <> show (A.sort nubA) <> "\ngot:     " <> show (A.sort (HashMap.toArrayBy Tuple m)))

  log "foldMapWithIndex (Data.Array.singleton <<< Tuple) agrees with toArrayBy"
  quickCheck $ \ (a :: Array (Tuple CollidingInt Int)) ->
    let m = HashMap.fromFoldable a
    in A.sort (foldMapWithIndex (\k v -> Array.singleton (Tuple k v)) m) === A.sort (HashMap.toArrayBy Tuple m)

  log "delete removes"
  quickCheck $ \ k v (a :: Array (Tuple CollidingInt String)) ->
    Nothing == (HashMap.lookup k $ HashMap.delete k $ HashMap.insert k v $ arbitraryHashMap a)

  log "delete idempotent"
  quickCheck $ \ k (a :: Array (Tuple CollidingInt String)) ->
    let m = arbitraryHashMap a in
    HashMap.delete k m == HashMap.delete k (HashMap.delete k m)

  log "delete preserves structure"
  quickCheck' 100000 $ \ k v (a :: Array (Tuple CollidingInt Boolean)) ->
    let m = arbitraryHashMap (A.filter (\t -> fst t /= k) a) in
    HashMap.delete k (HashMap.insert k v m) == m
    <?> ("k: " <> show k <>
         "\nv: " <> show v <>
         "\na: " <> show (A.filter (\t -> fst t /= k) a) <>
         "\nexpected: " <> show m <>
         "\ngot     : " <> show (HashMap.delete k (HashMap.insert k v m)) <>
         "\ninserted: " <> show (HashMap.insert k v m))

  log "delete preserves structure -- debugShow"
  quickCheck' 100000 $ \ k v (a :: Array (Tuple CollidingInt Boolean)) ->
    let m = arbitraryHashMap (A.filter (\t -> fst t /= k) a) in
    HashMap.debugShow (HashMap.delete k (HashMap.insert k v m)) == HashMap.debugShow m
    <?> ("k: " <> show k <>
         "\nv: " <> show v <>
         "\na: " <> show (A.filter (\t -> fst t /= k) a) <>
         "\nexpected: " <> show m <>
         "\ngot     : " <> show (HashMap.delete k (HashMap.insert k v m)) <>
         "\ninserted: " <> show (HashMap.insert k v m))

  log "fromFoldable (a <> b) = fromFoldable a <> fromFoldable b"
  quickCheck' 1000 \ a (b :: Array (Tuple CollidingInt String)) ->
    HashMap.fromFoldable (A.nubBy (\x y -> fst x `compare` fst y) (a <> b)) === HashMap.fromFoldable a <> HashMap.fromFoldable b

  log "map id = id"
  quickCheck \ (a :: Array (Tuple CollidingInt Boolean)) ->
    let m = arbitraryHashMap a in
    map identity m === m

  log "map (f <<< g) = map f <<< map g"
  quickCheck \ (f :: Int -> Int) (g :: Int -> Int) (a :: Array (Tuple CollidingInt Int)) ->
    let m = arbitraryHashMap a in
    map (f <<< g) m === map f (map g m)

  log "size = alaF Additive foldMap (const 1)"
  quickCheck \ (a :: Array (Tuple CollidingInt Int)) ->
    let m = arbitraryHashMap a in
    HashMap.size m === alaF Additive foldMap (const 1) m

  if Nothing == traverse (\x -> if x == 'a' then Just "bla" else Nothing) (HashMap.insert 5 'a' (HashMap.singleton 6 'z'))
    then log "traverse passed"
    else throw "traverse failed"

  if Just (HashMap.fromFoldable [Tuple (CollidingInt 100) 108, Tuple (CollidingInt 0) 6]) ==
     traverseWithIndex (\(CollidingInt k) v -> Just $ k + maybe 100 identity v) (HashMap.insert (CollidingInt 0) (Just 6) (HashMap.singleton (CollidingInt 100) (Just 8)))
    then log "traverse2 passed"
    else throw "traverse2 failed"

  log "sequence Just"
  quickCheck' 1000 $ \ (a :: Array (Tuple CollidingInt Int)) ->
    let m = arbitraryHashMap a
        m' = Just <$> m
    in Just m === sequence m'

  log "isEmpty"
  quickCheck' 1000 $ \ (a :: Array (Tuple CollidingInt Int)) ->
    let m = arbitraryHashMap a
        e = foldr (\(Tuple k _) m' -> HashMap.delete k m') m a
    in HashMap.isEmpty e

  log "Recheck previous failures:"
  nowGood (Just 85767) $ HashMap.lookup (-102839) (HashMap.insert (-102839) 85767 (HashMap.singleton (-717313) 472415))
  nowGood (Just (-533576)) $ HashMap.lookup (-635631) (HashMap.insert (-635631) (-533576) (HashMap.singleton 406135 (-940705)))
  nowGood (Just 957310) $ HashMap.lookup (-43745) (HashMap.insert (-43745) 957310 (HashMap.singleton 321662 955811))

  nowGood' t54
  nowGood' t105
  nowGood' t249

  log "Done."

t54 :: Boolean
t54 = let k = (-538828)
          v = false
          a = [(Tuple 605832 false),(Tuple 418793 false),(Tuple (-829612) false),(Tuple (-428805) true),(Tuple (-806480) true),(Tuple 637863 false),(Tuple (-616539) true),(Tuple (-650917) false),(Tuple 592866 false)]
          m = arbitraryHashMap a in
      HashMap.delete k (HashMap.insert k v m) == m

t105 :: Boolean
t105 = let k = (-354590)
           v = true
           a = [(Tuple (-438814) false)]
           m = arbitraryHashMap a in
       HashMap.delete k (HashMap.insert k v m) == m

t249 :: Boolean
t249 = let k = 855538
           v = false
           a = [(Tuple 359452 false),(Tuple 903388 false)]
           m = arbitraryHashMap a in
       HashMap.delete k (HashMap.insert k v m) == m
