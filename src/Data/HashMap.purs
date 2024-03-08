-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Data.HashMap (
  HashMap,

  empty,
  singleton,

  lookup,
  insert,
  delete,

  size,

  isEmpty,
  member,
  upsert,
  insertWith,
  update,
  alter,

  filter,
  filterWithKey,
  filterKeys,
  mapMaybe,
  mapMaybeWithKey,

  fromArray,
  fromFoldable,
  fromArrayBy,
  fromFoldableBy,
  fromFoldableWithIndex,
  toArrayBy,
  keys,
  values,

  union,
  unionWith,
  intersection,
  intersectionWith,
  difference,

  SemigroupHashMap(..),

  nubHash,

  debugShow,

  any
  ) where

import Prelude

import Data.Foldable (class Foldable, foldl, foldlDefault, foldr, foldrDefault)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex, foldlWithIndex, foldlWithIndexDefault, foldrWithIndexDefault)
import Data.Function.Uncurried (Fn2, Fn3, Fn4, Fn5, runFn2, runFn3, runFn4, runFn5)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Hashable (class Hashable, hash)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Newtype (class Newtype)
import Data.Traversable (class Traversable, traverse)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafePartial)

-- | Immutable hash maps from keys `k` to values `v`.
-- |
-- | Note that this is an *unordered* collection.
foreign import data HashMap :: Type -> Type -> Type

type role HashMap nominal representational

foreign import eqPurs :: forall k v. Fn2 (k -> k -> Boolean) (v -> v -> Boolean) (HashMap k v -> HashMap k v -> Boolean)

instance eqHashMap :: (Eq k, Eq v) => Eq (HashMap k v) where
  eq = runFn2 eqPurs eq eq

instance hashHashMap :: (Hashable k, Hashable v) => Hashable (HashMap k v) where
  hash = hashPurs (hash :: v -> Int)

foreign import hashPurs :: forall k v. (v -> Int) -> HashMap k v -> Int

instance monoidHashMap :: (Hashable k, Semigroup v) => Monoid (HashMap k v) where
  mempty = empty

instance semigroupHashMap :: (Hashable k, Semigroup v) => Semigroup (HashMap k v) where
  append = unionWith append

instance functorHashMap :: Functor (HashMap k) where
  map f = mapWithIndex (const f)

instance functorWithIndexHashMap :: FunctorWithIndex k (HashMap k) where
  mapWithIndex = mapWithIndexPurs

foreign import mapWithIndexPurs :: forall k v w. (k -> v -> w) -> HashMap k v -> HashMap k w

instance applyHashMap :: Hashable k => Apply (HashMap k) where
  apply = intersectionWith identity

instance bindHashMap :: Hashable k => Bind (HashMap k) where
  bind m f = mapMaybeWithKey (\k -> lookup k <<< f) m

-- | The `Foldable` instance is best used with a *commutative*
-- | function/`Monoid`, since hash maps do not guarantee any
-- | particular order.
instance foldableHashMap :: Foldable (HashMap k) where
  foldMap f = foldMapWithIndex (const f)
  foldr f = foldrDefault f
  foldl f = foldlDefault f

-- | The `FoldableWithIndex` instance is best used with a
-- | *commutative* function/`Monoid`, since hash maps do not guarantee
-- | any particular order.
instance foldableWithIndexHashMap :: FoldableWithIndex k (HashMap k) where
  foldMapWithIndex = foldMapWithIndexPurs mempty (<>)
  foldrWithIndex f = foldrWithIndexDefault f
  foldlWithIndex f = foldlWithIndexDefault f

-- First parameter is mempty, second is mappend
foreign import foldMapWithIndexPurs :: forall k v m. m -> (m -> m -> m) -> (k -> v -> m) -> HashMap k v -> m

instance traversableHashMap :: Traversable (HashMap k) where
  traverse f = traverseWithIndex (const f)
  sequence = traverse (\x -> x)

instance traversableWithIndexHashMap :: TraversableWithIndex k (HashMap k) where
  traverseWithIndex f m = traverseWithIndexPurs pure apply f m

foreign import traverseWithIndexPurs :: forall k v w m. (forall a. a -> m a) -> (forall a b. m (a -> b) -> m a -> m b) -> (k -> v -> m w) -> HashMap k v -> m (HashMap k w)


-- | This newtype provides a `Semigroup` instance for `HashMap k v`
-- | which delegates to the `Semigroup v` instance of elements. This
-- | newtype is deprecated and will be removed in the next major
-- | version. Use `HashMap` instead.
-- |
-- | We are currently in step 2 of the following migration process:
-- | 1. Add `SemigroupHashMap` with the new `Semigroup` instance and remove old instance from `HashMap`.
-- |
-- |    The new instance uses `unionWith append` instead of `union`.
-- |    You can recover the previous, left-biased behaviour by using
-- |    `SemigroupHashMap k (First v)` in place of `HashMap k v`.
-- |
-- | 2. Add new `Semigroup` instance to `HashMap` and deprecate `SemigroupHashMap`.
-- | 3. Remove `SemigroupHashMap`.
newtype SemigroupHashMap k v = SemigroupHashMap (HashMap k v)


derive instance newtypeSemigroupHashMap :: Newtype (SemigroupHashMap k v) _
derive newtype instance eqSemigroupHashMap :: (Eq k, Eq v) => Eq (SemigroupHashMap k v)
derive newtype instance hashSemigroupHashMap :: (Hashable k, Hashable v) => Hashable (SemigroupHashMap k v)
-- derive newtype instance monoidSemigroupHashMap :: Hashable k => Monoid (SemigroupHashMap k v)
derive newtype instance functorSemigroupHashMap :: Functor (SemigroupHashMap k)
derive newtype instance functorWithIndexSemigroupHashMap :: FunctorWithIndex k (SemigroupHashMap k)
derive newtype instance applySemigroupHashMap :: Hashable k => Apply (SemigroupHashMap k)
derive newtype instance bindSemigroupHashMap :: Hashable k => Bind (SemigroupHashMap k)
derive newtype instance foldableSemigroupHashMap :: Foldable (SemigroupHashMap k)
derive newtype instance foldableWithIndexSemigroupHashMap :: FoldableWithIndex k (SemigroupHashMap k)
derive newtype instance traversableSemigroupHashMap :: Traversable (SemigroupHashMap k)
derive newtype instance traversableWithIndexSemigroupHashMap :: TraversableWithIndex k (SemigroupHashMap k)
derive newtype instance showSemigroupHashMap :: (Show k, Show v) => Show (SemigroupHashMap k v)

instance semigroupSemigroupHashMap :: (Hashable k, Semigroup v) => Semigroup (SemigroupHashMap k v) where
  append (SemigroupHashMap l) (SemigroupHashMap r) = SemigroupHashMap (unionWith append l r)

instance monoidSemigroupHashMap :: (Hashable k, Semigroup v) => Monoid (SemigroupHashMap k v) where
  mempty = SemigroupHashMap empty

-- | The empty map.
foreign import empty :: forall k v. HashMap k v

foreign import lookupPurs :: forall k v. Fn5 (forall a. Maybe a) (forall a. a -> Maybe a) (k -> k -> Boolean) k Int (HashMap k v -> Maybe v)

-- | Get a value by key.
lookup :: forall k v. Hashable k => k -> HashMap k v -> Maybe v
lookup k = runFn5 lookupPurs Nothing Just (==) k (hash k)

foreign import insertPurs :: forall k v. Fn2 (k -> k -> Boolean) (k -> Int) (k -> v -> HashMap k v -> HashMap k v)

-- | Insert or replace a value.
-- |
-- | `lookup k (insert k v m) == Just v`
insert :: forall k v. Hashable k => k -> v -> HashMap k v -> HashMap k v
insert = runFn2 insertPurs (==) hash

foreign import insertWithPurs :: forall k v. Fn2 (k -> k -> Boolean) (k -> Int) ((v -> v -> v) -> k -> v -> HashMap k v -> HashMap k v)

-- | Insert the new value if the key doesn't exist, otherwise combine
-- | the existing and new values.
-- |
-- | The combining function is called with the existing value as the
-- | first argument and the new value as the second argument.
-- |
-- | ```PureScript
-- | insertWith (<>) 5 "b" (singleton 5 "a") == singleton 5 "ab"
-- | ```
-- |
-- | If your update function does not use the existing value, consider
-- | using `upsert` instead.
insertWith :: forall k v. Hashable k => (v -> v -> v) -> k -> v -> HashMap k v -> HashMap k v
insertWith = runFn2 insertWithPurs (==) hash

foreign import fromArrayPurs :: forall a k v. Fn2 (k -> k -> Boolean) (k -> Int) ((a -> k) -> (a -> v) -> Array a -> HashMap k v)

-- | Turn an array of pairs into a hash map.
-- |
-- | This uses a mutable hash map internally and is faster than
-- | `fromFoldable`.
-- |
-- | If you have an array of something other than tuples, use
-- | `fromArrayBy`.
fromArray :: forall k v. Hashable k => Array (Tuple k v) -> HashMap k v
fromArray = fromArrayBy fst snd

-- | Turn an array into a hash map given extraction functions for keys
-- | and values.
-- |
-- | This uses a mutable hash map internally and is faster than
-- | `fromFoldable` and `fromFoldableBy`.
fromArrayBy :: forall a k v. Hashable k => (a -> k) -> (a -> v) -> Array a -> HashMap k v
fromArrayBy = runFn2 fromArrayPurs (==) hash

-- TODO I really want to replace fromFoldable with a version that uses mutating insert, but to type it I need Traversable, I think.
-- | Turn a foldable functor of pairs into a hash map.
-- |
-- | In the presence of duplicate keys, later (by `foldl`) mappings
-- | overwrite earlier mappings.
-- |
-- | If your input is an array, consider using `fromArray` instead.
fromFoldable :: forall f k v. Foldable f => Hashable k => f (Tuple k v) -> HashMap k v
fromFoldable = foldl (\m (Tuple k v) -> insert k v m) empty

-- | Turn a foldable functor into a hash map given extraction
-- | functions for keys and values.
-- |
-- | If your input is an array, consider using `fromArrayBy` instead.
-- |
-- | `fromFoldableBy fst snd == fromFoldable`
fromFoldableBy :: forall f a k v. Foldable f => Hashable k => (a -> k) -> (a -> v) -> f a -> HashMap k v
fromFoldableBy kf vf = foldl (\m a -> insert (kf a) (vf a) m) empty

-- | Turn a foldable functor with index into a hash map.
-- |
-- | This can be used to convert, for example, an ordered map into a
-- | hash map with the same keys and values, or an array into a hash
-- | map with values indexed by their position in the array.
-- |
-- | ```PureScript
-- | fromFoldableWithIndex ["a", "b"] == fromArray [Tuple 0 "a", Tuple 1 "b"]
-- | ```
fromFoldableWithIndex :: forall f k v. FoldableWithIndex k f => Hashable k => f v -> HashMap k v
fromFoldableWithIndex = foldlWithIndex (\k m v -> insert k v m) empty

-- | Convert a map to an array using the given function.
-- |
-- | Note that no particular order is guaranteed.
-- |
-- | ```PureScript
-- | toArrayBy Tuple (singleton 1 2) == [Tuple 1 2]
-- | toArrayBy const        m == keys m
-- | toArrayBy (flip const) m == values m
-- | ```
foreign import toArrayBy :: forall a k v. (k -> v -> a) -> HashMap k v -> Array a

-- | Returns the keys of the map in no particular order.
-- |
-- | If you need both keys and values, use `toArrayBy` rather than
-- | both `keys` and `values`.
keys :: forall k v. HashMap k v -> Array k
keys = toArrayBy const

-- | Returns the values of the map in no particular order.
-- |
-- | If you need both keys and values, use `toArrayBy` rather than
-- | both `keys` and `values`.
values :: forall k v. HashMap k v -> Array v
values = toArrayBy (\_ v -> v)

foreign import deletePurs :: forall k v. Fn3 (k -> k -> Boolean) k Int (HashMap k v -> HashMap k v)

-- | Remove a key and its associated value from a map.
-- |
-- | `lookup k (delete k m) == Nothing`
delete :: forall k v. Hashable k => k -> HashMap k v -> HashMap k v
delete k = runFn3 deletePurs (==) k (hash k)

foreign import debugShow :: forall k v. HashMap k v -> String

instance showHashMap :: (Show k, Show v) => Show (HashMap k v) where
  show m = "(fromArray " <> show (toArrayBy Tuple m) <> ")"

foreign import singletonPurs :: forall k v. k -> Int -> v -> HashMap k v

-- | A map of one key and its associated value.
-- |
-- | `singleton k v == insert k v empty`
singleton :: forall k v. Hashable k => k -> v -> HashMap k v
singleton k = singletonPurs k (hash k)

-- | Test whether a map is empty.
-- |
-- | `isEmpty m  ==  (m == empty)`
foreign import isEmpty :: forall k v. HashMap k v -> Boolean

-- | Test whether a key is in a map.
member :: forall k v. Hashable k => k -> HashMap k v -> Boolean
member k = isJust <<< lookup k

-- | Insert a value, delete a value, or update a value for a key in a map.
alter :: forall k v. Hashable k => (Maybe v -> Maybe v) -> k -> HashMap k v -> HashMap k v
alter f k m = case f (lookup k m) of
  Nothing -> delete k m
  Just v -> insert k v m

-- | Update or delete the value for a key in a map.
update :: forall k v. Hashable k => (v -> Maybe v) -> k -> HashMap k v -> HashMap k v
update f = alter (_ >>= f)

-- | Insert a new value if it doesn't exist or update the existing
-- | value by applying a function to it.
-- |
-- | If you need to combine the new value with the existing value
-- | consider using `insertWith` instead.
upsert :: forall k v. Hashable k => (v -> v) -> k -> v -> HashMap k v -> HashMap k v
upsert f = insertWith (\v _ -> f v)

-- | Returns the number of key-value pairs in a map.
-- |
-- | `size (singleton k v) == 1`
foreign import size :: forall k v. HashMap k v -> Int

-- | Union two maps.
-- |
-- | For duplicate keys, we keep the value from the left map.
union :: forall k v. Hashable k => HashMap k v -> HashMap k v -> HashMap k v
union = runFn3 unionWithPurs eq hash const

foreign import unionWithPurs :: forall k v. Fn3 (k -> k -> Boolean) (k -> Int) (v -> v -> v) (HashMap k v -> HashMap k v -> HashMap k v)

-- | Union two maps, combining the values for keys that appear in both maps using the given function.
-- |
-- | `unionWith (-) (singleton 0 3) (singleton 0 2) == singleton 0 1`
unionWith :: forall k v. Hashable k => (v -> v -> v) -> HashMap k v -> HashMap k v -> HashMap k v
unionWith f = runFn3 unionWithPurs eq hash f

-- | Intersect two maps.
intersection :: forall k v. Hashable k => HashMap k v -> HashMap k v -> HashMap k v
intersection = intersectionWith (\_ x -> x)

foreign import intersectionWithPurs :: forall k a b c. Fn5 (forall x.Maybe x) (forall x.x -> Maybe x) (k -> k -> Boolean) (k -> Int) (a -> b -> c) (HashMap k a -> HashMap k b -> HashMap k c)

-- | Intersect two maps, combining the values for keys that appear in both maps using the given function.
-- |
-- | `intersectionWith (-) (singleton 0 3) (singleton 0 2) == singleton 0 1`
intersectionWith :: forall k a b c. Hashable k => (a -> b -> c) -> HashMap k a -> HashMap k b -> HashMap k c
intersectionWith f = runFn5 intersectionWithPurs Nothing Just eq hash f

-- | Compute the difference of two maps, that is a new map of all the
-- | mappings in the left map that do not have a corresponding key in
-- | the right map.
difference :: forall k v w. Hashable k => HashMap k v -> HashMap k w -> HashMap k v
difference l r = foldr delete l (keys r)

-- | Remove key-value-pairs from a map for which the predicate does
-- | not hold.
-- |
-- | ```PureScript
-- | filter (const False) m == empty
-- | filter (const True) m == m
-- | ```
filter :: forall k v. (v -> Boolean) -> HashMap k v -> HashMap k v
filter f = filterWithKey (const f)

-- | Remove key-value-pairs from a map for which the predicate does
-- | not hold.
-- |
-- | Like `filter`, but the predicate takes both key and value.
foreign import filterWithKey :: forall k v. (k -> v -> Boolean) -> HashMap k v -> HashMap k v

-- | Remove all keys from the map for which the predicate does not
-- | hold.
-- |
-- | `difference m1 m2 == filterKeys (\k -> member k m2) m1`
filterKeys :: forall k v. (k -> Boolean) -> HashMap k v -> HashMap k v
filterKeys f = filterWithKey (\k _ -> f k)

-- | Apply a function to all values in a hash map, discard the
-- | `Nothing` results, and keep the value of the `Just` results.
mapMaybe :: forall k v w. (v -> Maybe w) -> HashMap k v -> HashMap k w
mapMaybe = mapMaybeWithKey <<< const

-- | Apply a function to all key value pairs in a hash map, discard
-- | the `Nothing` results, and keep the value of the `Just` results.
mapMaybeWithKey :: forall k v w. (k -> v -> Maybe w) -> HashMap k v -> HashMap k w
mapMaybeWithKey f = map (unsafePartial fromJust) <<< filter isJust <<< mapWithIndex f
-- NOTE: Yes, this is three traversals but, perhaps suprisingly,
-- faster than building a new map by repeated insertion. Mutable
-- insertion is not going to make a big difference. If this ever
-- becomes a performance bottleneck, please open an issue --- this can
-- easily be done in one traversal.

-- | Remove duplicates from an array.
-- |
-- | Like `nub` from `Data.Array`, but uses a `Hashable` constraint
-- | instead of an `Ord` constraint.
nubHash :: forall a. Hashable a => Array a -> Array a
nubHash = runFn4 nubHashPurs Nothing Just (==) hash

foreign import nubHashPurs :: forall a. Fn4 (forall x. Maybe x) (forall x. x -> Maybe x) (a -> a -> Boolean) (a -> Int) (Array a -> Array a)

foreign import anyPurs :: forall k v. (v -> Boolean) -> HashMap k v -> Boolean

-- | Returns true if at least one HashMap element satisfies the given predicate, iterating the HashMap only as necessary and stopping as soon as the predicate yields true.
-- |
-- | Use this function instead of `Foldable.any` for more performance.
any :: forall k v. (v -> Boolean) -> HashMap k v -> Boolean
any = anyPurs
