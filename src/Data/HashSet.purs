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

  map,
  filter,
  mapMaybe,

  union,
  unions,
  intersection,
  difference,

  size,

  isEmpty,

  fromArray,
  fromFoldable,
  fromMap,
  toArray,
  toMap,
  toUnfoldable
  ) where

import Prelude hiding (map)

import Data.Array as Array
import Data.Foldable (class Foldable, foldr, fold)
import Data.FoldableWithIndex (foldMapWithIndex, foldlWithIndex, foldrWithIndex)
import Data.HashMap as M
import Data.Hashable (class Hashable)
import Data.Maybe (Maybe(..))
import Data.Unfoldable (class Unfoldable)

-- | A `HashSet a` is a set with elements of type `a`.
-- |
-- | `a` needs to be `Hashable` for most operations.
newtype HashSet a = HashSet (M.HashMap a Unit)

derive newtype instance eqHashSet :: Eq a => Eq (HashSet a)

derive newtype instance hashableHashSet :: Hashable a => Hashable (HashSet a)

-- derive newtype instance semigroupHashSet :: Hashable a => Semigroup (HashSet a)
instance semigroupHashSet :: Hashable a => Semigroup (HashSet a) where
  append = union

instance monoidHashSet :: Hashable a => Monoid (HashSet a) where
  mempty = empty

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

-- | Construct a new set by applying a function to each element of an
-- | input set.
-- |
-- | If distinct inputs map to the same output, this changes the
-- | cardinality of the set, therefore hash set is not a `Functor`.
-- | Also, the order in which elements appear in the new set is
-- | entirely dependent on the hash function for type `b`.
map :: forall a b. Hashable b => (a -> b) -> HashSet a -> HashSet b
map f = foldr (\x -> insert (f x)) empty

-- | Remove all elements from the set for which the predicate does not
-- | hold.
-- |
-- | `filter (const false) s == empty`
filter :: forall a. (a -> Boolean) -> HashSet a -> HashSet a
filter f (HashSet m) = HashSet (M.filterWithKey (\k _ -> f k) m)

-- | Map a function over a set, keeping only the `Just` values.
mapMaybe :: forall a b. Hashable b => (a -> Maybe b) -> HashSet a -> HashSet b
mapMaybe f =
  foldr (\a s -> case f a of
            Nothing -> s
            Just b -> insert b s) empty

-- | Union two sets.
union :: forall a. Hashable a => HashSet a -> HashSet a -> HashSet a
union (HashSet l) (HashSet r) = HashSet (M.unionWith const l r)

-- | Union a collection of sets.
unions :: forall f a. Foldable f => Hashable a => f (HashSet a) -> HashSet a
unions = fold

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

-- | Test whether a set is empty.
-- |
-- | `isEmpty s  ==  (s == empty)`
isEmpty :: forall a. HashSet a -> Boolean
isEmpty (HashSet m) = M.isEmpty m

-- | Turn an array into a hash set.
-- |
-- | This uses a mutable hash map internally and is faster than
-- | `fromFoldable`.
fromArray :: forall a. Hashable a => Array a -> HashSet a
fromArray = HashSet <<< M.fromArrayBy identity (const unit)

-- | Turn a set into an unfoldable functor.
-- |
-- | You probably want to use `toArray` instead, especially if you
-- | want to get an array out.
toUnfoldable :: forall f a. Unfoldable f => HashSet a -> f a
toUnfoldable = Array.toUnfoldable <<< toArray

toMap :: forall a. HashSet a -> M.HashMap a Unit
toMap (HashSet m) = m

fromMap :: forall a. M.HashMap a Unit -> HashSet a
fromMap = HashSet
