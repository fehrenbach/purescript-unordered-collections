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
  update,
  alter,

  fromFoldable,
  toArrayBy,
  keys,
  values,

  debugShow
  ) where

import Prelude

import Data.Foldable (class Foldable, foldl, foldlDefault, foldrDefault)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex, foldlWithIndexDefault, foldrWithIndex, foldrWithIndexDefault)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Hashable (class Hashable, hash)
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid (class Monoid, mempty)
import Data.Traversable (class Traversable, traverse)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (Tuple(..))

-- | Immutable hash maps from keys `k` to values `v`.
-- |
-- | Note that this is an *unordered* collection.
foreign import data HashMap :: Type -> Type -> Type

foreign import eqPurs :: forall k v. (k -> k -> Boolean) -> (v -> v -> Boolean) -> HashMap k v -> HashMap k v -> Boolean

instance eqHashMap :: (Eq k, Eq v) => Eq (HashMap k v) where
  eq = eqPurs eq eq

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

foreign import lookupPurs :: forall k v. Maybe v -> (v -> Maybe v) -> (k -> k -> Boolean) -> k -> Int -> HashMap k v -> Maybe v

-- | Get a value by key.
lookup :: forall k v. Hashable k => k -> HashMap k v -> Maybe v
lookup k = lookupPurs Nothing Just (==) k (hash k)

foreign import insertPurs :: forall k v. (k -> k -> Boolean) -> (k -> Int) -> k -> v -> HashMap k v -> HashMap k v

-- | Insert or replace a value.
-- |
-- | `lookup k (insert k v m) == Just v`
insert :: forall k v. Hashable k => k -> v -> HashMap k v -> HashMap k v
insert = insertPurs (==) hash

-- | Turn a foldable functor of pairs into a hash map.
-- |
-- | In the presence of duplicate keys, later (by `foldl`) mappings
-- | overwrite earlier mappings.
fromFoldable :: forall f k v. Foldable f => Hashable k => f (Tuple k v) -> HashMap k v
fromFoldable = foldl (\m (Tuple k v) -> insert k v m) empty

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

foreign import deletePurs :: forall k v. (k -> k -> Boolean) -> k -> Int -> HashMap k v -> HashMap k v

-- | Remove a key and its associated value from a map.
-- |
-- | `lookup k (delete k m) == Nothing`
delete :: forall k v. Hashable k => k -> HashMap k v -> HashMap k v
delete k = deletePurs (==) k (hash k)

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
union l r = foldrWithIndex insert r l -- TODO we can probably do better
