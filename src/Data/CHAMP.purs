module Data.CHAMP (
  CHAMP,

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
  values

  ) where

import Prelude

import Data.Foldable (class Foldable, foldl)
import Data.Hashable (class Hashable, hash)
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (Tuple(..))

-- | Immutable hash maps from keys `k` to values `v`.
-- |
-- | The implementation is based on "Optimizing Hash-Array Mapped
-- | Tries for Fast and Lean Immutable JVM Collections" (Steindorfer
-- | and Vinju, OOPSLA 2015, https://doi.org/10.1145/2814270.2814312).
foreign import data CHAMP :: Type -> Type -> Type

foreign import eqPurs :: forall k v. (k -> k -> Boolean) -> (v -> v -> Boolean) -> CHAMP k v -> CHAMP k v -> Boolean

instance eqCHAMP :: (Eq k, Eq v) => Eq (CHAMP k v) where
  eq = eqPurs eq eq

-- | The empty map.
foreign import empty :: forall k v. CHAMP k v

foreign import lookupPurs :: forall k v. Maybe v -> (v -> Maybe v) -> (k -> k -> Boolean) -> k -> Int -> CHAMP k v -> Maybe v

-- | Get a value by key.
lookup :: forall k v. Hashable k => k -> CHAMP k v -> Maybe v
lookup k = lookupPurs Nothing Just (==) k (hash k)

foreign import insertPurs :: forall k v. (k -> k -> Boolean) -> (k -> Int) -> k -> v -> CHAMP k v -> CHAMP k v

-- | Insert or replace a value.
-- |
-- | `lookup k (insert k v m) == Just v`
insert :: forall k v. Hashable k => k -> v -> CHAMP k v -> CHAMP k v
insert = insertPurs (==) hash

-- | Turn a foldable functor of pairs into a hash map.
-- |
-- | In the presence of duplicate keys, later (by `foldl`) mappings
-- | overwrite earlier mappings.
fromFoldable :: forall f k v. Foldable f => Hashable k => f (Tuple k v) -> CHAMP k v
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
foreign import toArrayBy :: forall a k v. (k -> v -> a) -> CHAMP k v -> Array a

-- | Returns the keys of the map in no particular order.
-- |
-- | If you need both keys and values, use `toArrayBy` rather than
-- | both `keys` and `values`.
keys :: forall k v. CHAMP k v -> Array k
keys = toArrayBy const

-- | Returns the values of the map in no particular order.
-- |
-- | If you need both keys and values, use `toArrayBy` rather than
-- | both `keys` and `values`.
values :: forall k v. CHAMP k v -> Array v
values = toArrayBy (\_ v -> v)

foreign import deletePurs :: forall k v. (k -> k -> Boolean) -> k -> Int -> CHAMP k v -> CHAMP k v

-- | Remove a key and its associated value from a map.
-- |
-- | `lookup k (delete k m) == Nothing`
delete :: forall k v. Hashable k => k -> CHAMP k v -> CHAMP k v
delete k = deletePurs (==) k (hash k)

foreign import debugShow :: forall k v. CHAMP k v -> String

instance showCHAMP :: Show (CHAMP k v) where
  show = debugShow

foreign import singletonPurs :: forall k v. k -> Int -> v -> CHAMP k v

-- | A map of one key and its associated value.
-- |
-- | `singleton k v == insert k v empty`
singleton :: forall k v. Hashable k => k -> v -> CHAMP k v
singleton k = singletonPurs k (hash k)

-- | Test whether a map is empty.
-- |
-- | `isEmpty m  ==  (m == empty)`
foreign import isEmpty :: forall k v. CHAMP k v -> Boolean

-- | Test whether a key is in a map.
member :: forall k v. Hashable k => k -> CHAMP k v -> Boolean
member k = isJust <<< lookup k

-- | Insert a value, delete a value, or update a value for a key in a map
alter :: forall k v. Hashable k => (Maybe v -> Maybe v) -> k -> CHAMP k v -> CHAMP k v
alter f k m = case f (lookup k m) of
  Nothing -> delete k m
  Just v -> insert k v m

-- | Update or delete the value for a key in a map
update :: forall k v. Hashable k => (v -> Maybe v) -> k -> CHAMP k v -> CHAMP k v
update f = alter (_ >>= f)

-- | Returns the number of key-value pairs in a map.
-- |
-- | `size (singleton k v) == 1`
foreign import size :: forall k v. CHAMP k v -> Int
