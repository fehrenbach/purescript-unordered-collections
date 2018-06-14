module Data.HashSet (
  HashSet,

  empty,
  insert,
  member
  ) where

import Prelude

import Data.Foldable (class Foldable, foldr)
import Data.HashMap as M
import Data.Hashable (class Hashable)

newtype HashSet a = HashSet (M.HashMap a Unit)

empty :: forall a. HashSet a
empty = HashSet M.empty

insert :: forall a. Hashable a => a -> HashSet a -> HashSet a
insert a (HashSet m) = HashSet (M.insert a unit m)

member :: forall a. Hashable a => a -> HashSet a -> Boolean
member a (HashSet m) = M.member a m

fromFoldable :: forall f a. Foldable f => Hashable a => f a -> HashSet a
fromFoldable = foldr insert empty
