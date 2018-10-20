-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Data.HashMap (
  HashMap,

  empty,
  singleton,

  lookup,
  insert,
  insertWith,
  delete,

  size,

  isEmpty,
  member,
  update,
  alter,

  filter,
  filterWithKey,
  filterKeys,

  fromArray,
  fromFoldable,
  fromArrayBy,
  fromFoldableBy,
  toArrayBy,
  keys,
  values,

  union,
  unionWith,
  intersection,
  intersectionWith,
  difference,

  nubHash,

  debugShow
  ) where

import Prelude

import Data.Foldable (class Foldable, foldl, foldlDefault, foldr, foldrDefault)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex, foldlWithIndexDefault, foldrWithIndexDefault)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Hashable (class Hashable, hash)
import Data.Maybe (Maybe(..), isJust)
import Data.Traversable (class Traversable, traverse)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (Tuple(..), fst, snd)

-- | Immutable hash maps from keys `k` to values `v`.
-- |
-- | Note that this is an *unordered* collection.
foreign import data HashMap :: Type -> Type -> Type

foreign import eqPurs :: forall k v. Fn2 (k -> k -> Boolean) (v -> v -> Boolean) (HashMap k v -> HashMap k v -> Boolean)

instance eqHashMap :: (Eq k, Eq v) => Eq (HashMap k v) where
  eq = runFn2 eqPurs eq eq

instance hashHashMap :: (Hashable k, Hashable v) => Hashable (HashMap k v) where
  hash = hashPurs (hash :: v -> Int)

foreign import hashPurs :: forall k v. (v -> Int) -> HashMap k v -> Int

instance monoidHashMap :: Hashable k => Monoid (HashMap k v) where
  mempty = empty

-- | This is "the shallow" semigroup instance, where maps themselves
-- | are combined using `union` rather than elements being combined.
-- | For duplicate keys, values from the left map are preserved.
instance semigroupHashMap :: Hashable k => Semigroup (HashMap k v) where
  append = union

instance functorHashMap :: Functor (HashMap k) where
  map f = mapWithIndex (const f)

instance functorWithIndexHashMap :: FunctorWithIndex k (HashMap k) where
  mapWithIndex = mapWithIndexPurs

foreign import mapWithIndexPurs :: forall k v w. (k -> v -> w) -> HashMap k v -> HashMap k w

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

-- | The empty map.
foreign import empty :: forall k v. HashMap k v

foreign import lookupPurs :: forall k v. Fn3 (k -> k -> Boolean) k Int (HashMap k v -> Maybe v)

-- | Get a value by key.
lookup :: forall k v. Hashable k => k -> HashMap k v -> Maybe v
lookup k = runFn3 lookupPurs (==) k (hash k)

foreign import insertPurs :: forall k v. Fn2 (k -> k -> Boolean) (k -> Int) (k -> v -> HashMap k v -> HashMap k v)

-- | Insert or replace a value.
-- |
-- | `lookup k (insert k v m) == Just v`
insert :: forall k v. Hashable k => k -> v -> HashMap k v -> HashMap k v
insert = runFn2 insertPurs (==) hash

foreign import insertWithPurs :: forall k v. Fn2 (k -> k -> Boolean) (k -> Int) ((v -> v -> v) -> k -> v -> HashMap k v -> HashMap k v)

-- | Insert or update a value with the given function.
-- |
-- | The combining function is called with the existing value as the
-- | first argument and the new value as the second argument.
-- |
-- | ```PureScript
-- | insertWith (<>) 5 "b" (singleton 5 "a") == singleton 5 "ab"
-- | ```
insertWith :: forall k v. Hashable k => (v -> v -> v) -> k -> v -> HashMap k v -> HashMap k v
insertWith = runFn2 insertWithPurs (==) hash


-- | Turn an array of pairs into a hash map.
-- |
-- | This uses a mutable hash map internally and is faster than
-- | `fromFoldable`.
-- |
-- | If you have an array of something other than tuples, use
-- | `fromArrayBy`.
fromArray :: forall k v. Hashable k => Array (Tuple k v) -> HashMap k v
fromArray = fromArrayBy fst snd

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

foreign import fromArrayPurs :: forall a k v. Fn2 (k -> k -> Boolean) (k -> Int) ((a -> k) -> (a -> v) -> Array a -> HashMap k v)

-- | Turn an array into a hash map given extraction functions for keys
-- | and values.
-- |
-- | This uses a mutable hash map internally and is faster than
-- | `fromFoldable` and `fromFoldableBy`.
fromArrayBy :: forall a k v. Hashable k => (a -> k) -> (a -> v) -> Array a -> HashMap k v
fromArrayBy = runFn2 fromArrayPurs (==) hash

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
  show m = "(fromFoldable " <> show (toArrayBy Tuple m) <> ")"

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

-- | Insert a value, delete a value, or update a value for a key in a map
alter :: forall k v. Hashable k => (Maybe v -> Maybe v) -> k -> HashMap k v -> HashMap k v
alter f k m = case f (lookup k m) of
  Nothing -> delete k m
  Just v -> insert k v m

-- | Update or delete the value for a key in a map
update :: forall k v. Hashable k => (v -> Maybe v) -> k -> HashMap k v -> HashMap k v
update f = alter (_ >>= f)

-- | Returns the number of key-value pairs in a map.
-- |
-- | `size (singleton k v) == 1`
foreign import size :: forall k v. HashMap k v -> Int

-- | Union two maps.
-- |
-- | For duplicate keys, we keep the value from the left map.
-- |
-- | This is the same as `Semigroup.append` aka `(<>)`.
union :: forall k v. Hashable k => HashMap k v -> HashMap k v -> HashMap k v
union = runFn3 unionWithPurs eq hash const

foreign import unionWithPurs :: forall k v. Fn3 (k -> k -> Boolean) (k -> Int) (v -> v -> v) (HashMap k v -> HashMap k v -> HashMap k v)

-- | Union two maps, combining the values for keys that appear in both maps using the given function.
-- |
-- | `unionWith (-) (singleton 0 3) (singleton 0 2) == singleton 0 1`
unionWith :: forall k v. Hashable k => (v -> v -> v) -> HashMap k v -> HashMap k v -> HashMap k v
unionWith f = runFn3 unionWithPurs eq hash f

-- | Intersect two maps.
-- |
-- | For duplicate keys, we keep the value from the right map.
-- |
-- | This is the same as `Semigroup.append` aka `(<>)`.
intersection :: forall k v. Hashable k => HashMap k v -> HashMap k v -> HashMap k v
intersection = runFn3 intersectionWithPurs eq hash (\_ x -> x)

foreign import intersectionWithPurs :: forall k v. Fn3 (k -> k -> Boolean) (k -> Int) (v -> v -> v) (HashMap k v -> HashMap k v -> HashMap k v)

-- | Intersect two maps, combining the values for keys that appear in both maps using the given function.
-- |
-- | `intersectionWith (-) (singleton 0 3) (singleton 0 2) == singleton 0 1`
intersectionWith :: forall k v. Hashable k => (v -> v -> v) -> HashMap k v -> HashMap k v -> HashMap k v
intersectionWith f = runFn3 intersectionWithPurs eq hash f

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
filter :: forall k v. Hashable k => (v -> Boolean) -> HashMap k v -> HashMap k v
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
filterKeys :: forall k v. Hashable k => (k -> Boolean) -> HashMap k v -> HashMap k v
filterKeys f = filterWithKey (\k v -> f k)

-- | Remove duplicates from an array.
-- |
-- | Like `nub` from `Data.Array`, but uses a `Hashable` constraint
-- | instead of an `Ord` constraint.
nubHash :: forall a. Hashable a => Array a -> Array a
nubHash = runFn2 nubHashPurs (==) hash

foreign import nubHashPurs :: forall a. Fn2 (a -> a -> Boolean) (a -> Int) (Array a -> Array a)
