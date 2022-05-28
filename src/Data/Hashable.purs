-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Data.Hashable (
  class Hashable,
  hash,

  -- HashableRecord stuff
  class HashableRecord,
  hashRecord
) where

import Prelude

import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.Eq (class EqRecord)
import Data.Foldable (class Foldable, foldl)
import Data.Int.Bits ((.^.))
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil)
import Record (get)
import Type.Prelude (class IsSymbol)
import Type.Proxy (Proxy(..))

-- | The `Hashable` type class represents types with decidable
-- | equality and a hash function for use in hash-based algorithms and
-- | data structures, not cryptography.
-- |
-- | Instances of `Hashable` must satisfy the following law:
-- |
-- | ```PureScript
-- | (a == b) `implies` (hash a == hash b)
-- | ```
-- |
-- | Note that while `hash = const 0` is a law-abiding implementation,
-- | one would usually prefer more discrimination. Hash-based data
-- | structures and algorithms tend to perform badly in the presence
-- | of excessive numbers of collisions.
-- |
-- | Hash values produced by `hash` should not be relied upon to be
-- | stable accross multiple executions of a program and should not be
-- | stored externally. While we currently do not do this, we might
-- | want to use a fresh salt for every execution in the future.
class Eq a <= Hashable a where
  hash :: a -> Int

instance hashableBoolean :: Hashable Boolean where
  hash false = 0
  hash true = 1

instance hashableInt :: Hashable Int where
  hash n = n

foreign import hashNumber :: Number -> Int

instance hashableNumber :: Hashable Number where
  hash = hashNumber

instance hashableChar :: Hashable Char where
  hash = fromEnum

foreign import hashString :: String -> Int

instance hashableString :: Hashable String where
  hash = hashString

hashFoldable :: forall f a. Foldable f => Hashable a => f a -> Int
hashFoldable = foldl (\h a -> 31 * h + hash a) 1

instance hashableArray :: Hashable a => Hashable (Array a) where
  hash = hashFoldable

instance hashableList :: Hashable a => Hashable (List a) where
  hash = hashFoldable

instance hashableTuple :: (Hashable a, Hashable b) => Hashable (Tuple a b) where
  hash (Tuple a b) = hash a * 31 + hash b

instance hashableMaybe :: Hashable a => Hashable (Maybe a) where
  hash Nothing = 0
  hash (Just a) = 1 + hash a

instance hashableEither :: (Hashable a, Hashable b) => Hashable (Either a b) where
  hash (Left x) = hash x .^. 0x55555555
  hash (Right x) = hash x

instance hashableUnit :: Hashable Unit where
  hash _ = 1

instance hashableVoid :: Hashable Void where
  hash _ = 0

-- BoundedEnums have `fromEnum` as a (perfect, even) hash function,
-- but the below instance is overlapping with all others and thus
-- "breaks" type class resolution. The instance chains paper mentions
-- encoding default implementations but does not give details. I don't
-- know whether that requires backtracking and/or explicit failure,
-- both of which are not implemented in PureScript (0.12).

-- instance hashableBoundedEnum :: BoundedEnum a => Hashable a where
--   hash = fromEnum

class EqRecord l r <= HashableRecord l r | l -> r where
  hashRecord :: Proxy l -> Record r -> Int

instance hashableRecordNil :: HashableRecord Nil r where
  hashRecord _ _ = 0

instance hashableRecordCons ::
  ( Hashable vt
  , HashableRecord tl r
  , IsSymbol l
  , Row.Cons l vt whatev r
  ) => HashableRecord (Cons l vt tl) r where
  hashRecord _ record = hash (get (Proxy :: Proxy l) record) * 31 + hashRecord (Proxy :: Proxy tl) record

instance hashableRecord ::
  (RowToList r l, HashableRecord l r, EqRecord l r)
  => Hashable (Record r) where
  hash = hashRecord (Proxy :: Proxy l)

-- TODO add combinators and a generics-rep implementation
