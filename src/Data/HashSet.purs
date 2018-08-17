-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Data.HashSet (
  HashSet,

  empty,
  singleton,

  insert,
  member,
  delete,

  union,
  intersection,
  difference,

  size,

  fromFoldable,
  toArray
  ) where

import Prelude

import Data.Foldable (class Foldable, foldr)
import Data.FoldableWithIndex (foldMapWithIndex, foldlWithIndex, foldrWithIndex)
import Data.HashMap as M
import Data.Hashable (class Hashable)

-- | A `HashSet a` is a set with elements of type `a`.
-- |
-- | `a` needs to be `Hashable` for most operations.
newtype HashSet a = HashSet (M.HashMap a Unit)

derive newtype instance eqHashSet :: Eq a => Eq (HashSet a)

instance showHashSet :: Show a => Show (HashSet a) where
  show s = "(fromFoldable " <> show (toArray s) <> ")"

-- | The `Foldable` instance is best used with a *commutative*
-- | function/`Monoid`, since hash sets do not guarantee any
-- | particular order.
instance foldableHashSet :: Foldable HashSet where
  foldr f a (HashSet m) = foldrWithIndex (\k _ -> f k) a m
  foldl f a (HashSet m) = foldlWithIndex (\k b _ -> f b k) a m
  foldMap f (HashSet m) = foldMapWithIndex (\k _ -> f k) m

-- | The empty set.
empty :: forall a. HashSet a
empty = HashSet M.empty

-- | The singleton set.
singleton :: forall a. Hashable a => a -> HashSet a
singleton a = HashSet (M.singleton a unit)

-- | Insert a value into a set.
insert :: forall a. Hashable a => a -> HashSet a -> HashSet a
insert a (HashSet m) = HashSet (M.insert a unit m)

-- | Test whether a value is in a set.
member :: forall a. Hashable a => a -> HashSet a -> Boolean
member a (HashSet m) = M.member a m

-- | Remove a value from a set.
delete :: forall a. Hashable a => a -> HashSet a -> HashSet a
delete a (HashSet m) = HashSet (M.delete a m)

-- | Create a set from a foldable structure.
fromFoldable :: forall f a. Foldable f => Hashable a => f a -> HashSet a
fromFoldable = foldr insert empty

-- | Turn a set into an array of its elments in no particular order.
-- |
-- | To delete duplicates in an array, consider using `nubHash` from
-- | `Data.HashMap` instead of `toArray <<< fromFoldable`.
toArray :: forall a. HashSet a -> Array a
toArray (HashSet m) = M.keys m

-- | Union two sets.
union :: forall a. Hashable a => HashSet a -> HashSet a -> HashSet a
union (HashSet l) (HashSet r) = HashSet (M.unionWith const l r)

-- | Intersect two sets.
intersection :: forall a. Hashable a => HashSet a -> HashSet a -> HashSet a
intersection (HashSet l) (HashSet r) = HashSet (M.intersectionWith const l r)

-- | Difference of two sets.
-- |
-- | Also known as set minus or relative complement. Returns a set of
-- | all elements of the left set that are not in the right set.
difference :: forall a. Hashable a => HashSet a -> HashSet a -> HashSet a
difference (HashSet l) (HashSet r) = HashSet (M.difference l r)

-- | Count the number of elements.
-- |
-- | Also known as cardinality, or length.
size :: forall a. HashSet a -> Int
size (HashSet m) = M.size m
