module Data.CHAMP (
  CHAMP,

  empty,
  -- isEmpty,

  singleton,

  lookup,
  insert,
  delete,

  fromFoldable,
  toArrayBy,
  keys,
  values

  ) where

import Prelude

import Data.Foldable (class Foldable, foldr)
import Data.Hashable (class Hashable, hash)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

foreign import data CHAMP :: Type -> Type -> Type

foreign import eqPurs :: forall k v. (k -> k -> Boolean) -> (v -> v -> Boolean) -> CHAMP k v -> CHAMP k v -> Boolean

instance eqCHAMP :: (Eq k, Eq v) => Eq (CHAMP k v) where
  eq = eqPurs eq eq

foreign import empty :: forall k v. CHAMP k v

foreign import lookupPurs :: forall k v. Maybe v -> (v -> Maybe v) -> (k -> k -> Boolean) -> k -> Int -> CHAMP k v -> Maybe v

lookup :: forall k v. Hashable k => k -> CHAMP k v -> Maybe v
lookup k = lookupPurs Nothing Just (==) k (hash k)

foreign import insertPurs :: forall k v. (k -> k -> Boolean) -> (k -> Int) -> k -> v -> CHAMP k v -> CHAMP k v

insert :: forall k v. Hashable k => k -> v -> CHAMP k v -> CHAMP k v
insert = insertPurs (==) hash

fromFoldable :: forall f k v. Foldable f => Hashable k => f (Tuple k v) -> CHAMP k v
fromFoldable = foldr (\(Tuple k v) -> insert k v) empty

foreign import toArrayBy :: forall a k v. (k -> v -> a) -> CHAMP k v -> Array a

keys :: forall k v. CHAMP k v -> Array k
keys = toArrayBy const

values :: forall k v. CHAMP k v -> Array v
values = toArrayBy (\_ v -> v)

foreign import deletePurs :: forall k v. (k -> k -> Boolean) -> k -> Int -> CHAMP k v -> CHAMP k v

delete :: forall k v. Hashable k => k -> CHAMP k v -> CHAMP k v
delete k = deletePurs (==) k (hash k)

foreign import debugShow :: forall k v. CHAMP k v -> String

instance showCHAMP :: Show (CHAMP k v) where
  show = debugShow

foreign import singletonPurs :: forall k v. k -> Int -> v -> CHAMP k v

singleton :: forall k v. Hashable k => k -> v -> CHAMP k v
singleton k = singletonPurs k (hash k)

-- Can't have isEmpty with that implementation with that signature..
-- isEmpty :: forall k v. CHAMP k v -> Boolean
-- isEmpty = (empty == _)
