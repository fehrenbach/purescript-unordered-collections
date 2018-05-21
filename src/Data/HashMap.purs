module Data.HashMap (
  HashMap,
  
  empty,
  singleton,

  lookup,
  insert,
  delete,

  isEmpty,
  member,
  size,

  update,
  alter,
  union,

  keys,
  values,

  fromFoldable,
  fromFoldableBy,
  toUnfoldableBy,
  toUnfoldableUnordered
  ) where

import Prelude

import Data.Array as A
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldMap, foldl, foldlDefault, foldrDefault)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex, foldlWithIndexDefault, foldrWithIndexDefault)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Hashable (class Hashable, hash)
import Data.Int (binary, toStringAs)
import Data.Int.Bits (complement, shl, zshr, (.&.), (.|.))
import Data.List (List(..), (:))
import Data.List as L
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Monoid (class Monoid)
import Data.Traversable (class Traversable, sequence, traverse)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, unfoldr)

-- TODO consider unifying Singleton and Collision by either getting
-- rid of singleton nodes, or linking singleton nodes directly to
-- another singleton node
-- TODO consider heterogenous arrays, like the original Clojure
-- implementation or even like Steindorfer & Vinju (OOPSLA'15)
-- | An immutable `HashMap k v` maps `Hashable` keys of type `k` to
-- | values of type `v`.
-- |
-- | The implementation uses a hash array mapped trie with path
-- | copying on updates and path compression on deletions. In the
-- | absence of hash collisions, common operations are O(log32 n).
data HashMap k v
  = Singleton Int k v
  | Collision Int (Array {k::k, v::v}) -- Invariant: array has at least 2 elements
  | Bitmapped Int (Array (HashMap k v))

instance eqHashMap :: (Eq k, Eq v) => Eq (HashMap k v) where
  eq (Singleton h1 k1 v1) (Singleton h2 k2 v2) = h1 == h2 && k1 == k2 && v1 == v2
  eq (Collision h1 l1) (Collision h2 l2) =
    h1 == h2
    && L.all (\{k:k1,v:v1} -> isJust (L.find (\{k:k2, v:v2} -> k1 == k2 && v1 == v2) l2)) l1
    && L.all (\{k:k2,v:v2} -> isJust (L.find (\{k:k1, v:v1} -> k1 == k2 && v1 == v2) l1)) l2
  eq (Bitmapped h1 a1) (Bitmapped h2 a2) = h1 == h2 && a1 == a2
  eq _ _ = false

instance eq1HashMap :: Eq k => Eq1 (HashMap k) where
  eq1 = eq

instance monoidHashMap :: Hashable k => Monoid (HashMap k v) where
  mempty = empty

instance semigroupHashMap :: Hashable k => Semigroup (HashMap k v) where
  append = union

instance functorHashMap :: Functor (HashMap k) where
  map f (Singleton h k v) = Singleton h k (f v)
  map f (Collision h l) = Collision h (map (\{k, v} -> {k, v: f v}) l)
  map f (Bitmapped b a) = Bitmapped b (map (map f) a)

instance functorWithIndexHashMap :: FunctorWithIndex k (HashMap k) where
  mapWithIndex f (Singleton h k v) = Singleton h k (f k v)
  mapWithIndex f (Collision h l) = Collision h (map (\{k, v} -> {k, v: f k v}) l)
  mapWithIndex f (Bitmapped b a) = Bitmapped b (map (mapWithIndex f) a)

instance foldableHashMap :: Foldable (HashMap k) where
  foldMap f (Singleton _ _ v) = f v
  foldMap f (Collision _ l) = foldMap (f <<< _.v) l
  foldMap f (Bitmapped _ a) = foldMap (foldMap f) a
  -- TODO HashMaps are unordered, so order hardly matters, but we
  -- could be more efficient, probably. Possibly even more efficient
  -- than foldMap, because we don't need the Monoid dictionary
  foldr f = foldrDefault f
  foldl f = foldlDefault f

instance foldableWithIndexHashMap :: FoldableWithIndex k (HashMap k) where
  foldMapWithIndex f (Singleton _ k v) = f k v
  foldMapWithIndex f (Collision _ l) = foldMap (\t -> f t.k t.v) l
  foldMapWithIndex f (Bitmapped _ a) = foldMap (foldMapWithIndex f) a
  foldrWithIndex f = foldrWithIndexDefault f
  foldlWithIndex f = foldlWithIndexDefault f

instance traversableHashMap :: Traversable (HashMap k) where
  sequence (Singleton h k v) = pure (Singleton h k) <*> v
  sequence (Collision h l) =
    pure (Collision h) <*> sequence (map (\{k, v} ->
                                           (pure (\a -> {k, v:a})) <*> v) l)
  sequence (Bitmapped bm a) =
    pure (Bitmapped bm) <*> sequence (map sequence a)

  traverse f (Singleton h k v) = Singleton h k <$> f v
  traverse f (Collision h l) =
    Collision h <$> traverse (\{k, v} -> (\a -> {k, v: a}) <$> f v) l
  traverse f (Bitmapped b a) =
    Bitmapped b <$> traverse (traverse f) a

instance traversableWithIndexHashMap :: TraversableWithIndex k (HashMap k) where
  traverseWithIndex f (Singleton h k v) = Singleton h k <$> f k v
  traverseWithIndex f (Collision h l) =
    Collision h <$> traverse (\{k, v} -> (\a -> {k, v: a}) <$> f k v) l
  traverseWithIndex f (Bitmapped b a) =
    Bitmapped b <$> traverse (traverseWithIndex f) a

-- TODO remove/change to non-debugging representation
instance showHashMap :: (Show k, Show v) => Show (HashMap k v) where
  show (Singleton h k v) = "(Singleton " <> show h <> " " <> show k <> " " <> show v <> ")"
  show (Collision h l) = "(Collision " <> show h <> " " <> show (map (\ {k,v} -> "(Tuple " <> show k <> " " <> show v <> ")") l) <> ")"
  show (Bitmapped i a) = "(Bitmapped " <> toStringAs binary i <> " " <> show a <> ")"

empty :: forall k v. HashMap k v
empty = Bitmapped 0 []

isEmpty :: forall k v. HashMap k v -> Boolean
isEmpty (Bitmapped 0 _{- should always be [] -}) = true
isEmpty _ = false

singleton :: forall k v. Hashable k => k -> v -> HashMap k v
singleton k = Singleton (hash k) k

insert :: forall k v. Hashable k => k -> v -> HashMap k v -> HashMap k v
insert k v m = insertImpl m k v (hash k) 0

insertImpl :: forall k v. Hashable k => HashMap k v -> k -> v -> Int -> Int -> HashMap k v
insertImpl node key value hash' s = case node of
  Singleton h k v ->
    if hash' == h
    then if key == k
         then Singleton h k value
         else Collision h [{k,v}, {k:key, v: value}]
    else joinImpl s (Singleton hash' key value) node
  Collision h l ->
    if hash' == h
    then case A.findIndex (\{k} -> k == key) l of
      -- TODO cons or snoc?
      Nothing -> Collision h (A.cons {k: key, v: value} l)
      Just i -> Collision h (unsafeUpdateAt i {k: key, v: value} l)
    else joinImpl s (Singleton hash' key value) node
  Bitmapped 0 _ -> Singleton hash' key value
  Bitmapped bm a ->
    let bit = 1 `shl` ((hash' `zshr` s) .&. 31)
        i = popCount (bm .&. (bit - 1))
    in if bm .&. bit == 0
       then Bitmapped (bm .|. bit) (unsafeInsertAt i (Singleton hash' key value) a)
       else Bitmapped bm (unsafeUpdateAt i (insertImpl (unsafeArrayIndex a i) key value hash' (s+5)) a)

lookup :: forall k v. Hashable k => k -> HashMap k v -> Maybe v
lookup k m = lookupImpl k m (hash k) 0

lookupImpl :: forall k v. Hashable k => k -> HashMap k v -> Int -> Int -> Maybe v
lookupImpl key node hash' s = case node of
  Singleton h k v -> if h == hash' && k == key then Just v else Nothing
  Collision h l -> if h == hash'
                   then _.v <$> L.find (\{k} -> k == key) l
                   else Nothing
  Bitmapped 0 _ -> Nothing
  Bitmapped bm a ->
    let bit = 1 `shl` ((hash' `zshr` s) .&. 31)
    in if bm .&. bit == 0
       then Nothing
       else lookupImpl key (unsafeArrayIndex a (popCount (bm .&. (bit-1)))) hash' (s+5)

member :: forall k v. Hashable k => k -> HashMap k v -> Boolean
member k = isJust <<< lookup k

-- Modest performance gain from using these over the Data.Array
-- versions + unsafePartial fromJust
foreign import unsafeArrayIndex :: forall a. Array a -> Int -> a
foreign import unsafeInsertAt :: forall a. Int -> a -> Array a -> Array a
foreign import unsafeUpdateAt :: forall a. Int -> a -> Array a -> Array a
foreign import unsafeDeleteAt :: forall a. Int -> Array a -> Array a

-- TODO submit PR to Data.Int.Bits
foreign import popCount :: Int -> Int

unsafeGetStoredHash :: forall k v. HashMap k v -> Int
unsafeGetStoredHash (Singleton h _ _) = h
unsafeGetStoredHash (Collision h _) = h
unsafeGetStoredHash (Bitmapped _ _) = 0

-- join two non-bitmapped nodes into one bitmapped node
joinImpl :: forall k v. Int -> HashMap k v -> HashMap k v -> HashMap k v
joinImpl s n m =
  let nh = 1 `shl` (((unsafeGetStoredHash n) `zshr` s) .&. 31)
      mh = 1 `shl` (((unsafeGetStoredHash m) `zshr` s) .&. 31)
      bm = nh .|. mh
  in Bitmapped bm (if nh == mh then [joinImpl (s + 5) n m]
                   else if (nh `zshr` 0) < (mh `zshr` 0) -- compare as unsigned
                        then [n, m] else [m, n])

-- | Convert a `Foldable` of key-value `Tuple`s to a map.
-- | For duplicate keys, later (by `foldl`) values overwrite earlier values.
fromFoldable :: forall f k v. Foldable f => Hashable k => f (Tuple k v) -> HashMap k v
fromFoldable = foldl (\m (Tuple k v) -> insert k v m) empty

-- | Convert a `Foldable` to a map by extracting keys and values.
-- | For duplicate keys, later (by `foldl`) values overwrite earlier values.
-- |
-- |     fromFoldable ≡ fromFoldableBy fst snd
-- |
-- |     fromFoldableBy kf vf fa ≡ foldMap (\a -> singleton (kf a) (vf a)) fa
-- |     fromFoldableBy kf vf fa ≡ fromFoldable (map (a -> Tuple (kf a) (vf a)) fa)
fromFoldableBy :: forall f a k v. Foldable f => Hashable k => (a -> k) -> (a -> v) -> f a -> HashMap k v
fromFoldableBy kf vf = foldl (\m a -> insert (kf a) (vf a) m) empty

-- TODO maybe add indexedBy :: f a -> (a -> k) -> HashMap k a

-- | Convert a map to an `Unfoldable` of `Tuple`s.
-- | No particular ordering is guaranteed.
toUnfoldableUnordered :: forall f k v. Unfoldable f => HashMap k v -> f (Tuple k v)
toUnfoldableUnordered = toUnfoldableBy Tuple

{-
-- TODO write a version of unfoldable specialized to arrays. In theory
-- we could save a lot of allocation by taking a mutable array and
-- traversing the map once, pushing elements onto the array. In
-- practice, all of the traversal implementations (foldMapWithKey,
-- traverseWithKey, ...) suck badly. Traverse rebuilds the whole map
-- structure. Not sure why foldMap is so bad. I guess the monoid (of
-- Eff) stuff?

toArrayUnordered :: forall k v. HashMap k v -> Array (Tuple k v)
toArrayUnordered m = pureST do
  arr <- emptySTArray
  -- 183 µs
  -- arr' <- foldWithIndexM (\k a v -> void $ pushSTArray a (Tuple k v) >>= const (pure a)) arr m
  -- 283 µs
  -- _ <- foldMapWithIndex (\k v -> void $ pushSTArray arr (Tuple k v)) m
  -- 215 µs
  -- _ <- traverseWithIndex_ (\k v -> pushSTArray arr (Tuple k v)) m
  unsafeFreeze arr
-}

-- | Returns the keys of the map in no particular order.
keys :: forall f k v. Unfoldable f => HashMap k v -> f k
keys = toUnfoldableBy const

-- | Returns the values of the map in no particular order.
values :: forall f k v. Unfoldable f => HashMap k v -> f v
values = toUnfoldableBy (flip const)

-- | Convert a HashMap to an unfoldable functor, like `Array` or `List`
-- |
-- | This takes a function from key and value to the element type of
-- | the result. For example, fixing the functor to `Array`, passing
-- | `Tuple` will result in an array of key-value pairs.
-- |
-- | ```PureScript
-- | toUnfoldableBy Tuple        m == toUnfoldableUnordered m
-- | toUnfoldableBy const        m == keys m
-- | toUnfoldableBy (flip const) m == values m
-- | ```
toUnfoldableBy :: forall f a k v. Unfoldable f => (k -> v -> a) -> HashMap k v -> f a
toUnfoldableBy f m = unfoldr go (m : Nil) where
  go Nil = Nothing
  go (hd : tl) = case hd of
    Singleton _ k v -> Just (Tuple (f k v) tl)
    -- We build a singleton node here, which needs a hash. We don't
    -- look at it later, so we just use 0. This is not pretty, but
    -- allows us to get by without a `Hashable k` constraint.
    Collision _ l -> go ((A.toUnfoldable $ map (\{k, v} -> Singleton 0 k v) l) <> tl)
    Bitmapped _ a -> go (L.fromFoldable a <> tl)

delete :: forall k v. Hashable k => k -> HashMap k v -> HashMap k v
delete k m = deleteImpl k (hash k) 0 m

deleteImpl :: forall k v. Hashable k => k -> Int -> Int -> HashMap k v -> HashMap k v
deleteImpl k hash' s n@(Singleton h k' _) =
  if h == hash' && k == k'
  then empty else n
deleteImpl k hash' s n@(Collision h l) =
  if h /= hash' then n else case A.filter (\{k:k'} -> k /= k') l of
    -- Nil -> unsafeCrashWith "Either filter removed more than one entry or a Collision node had less than two entries to begin with. Both of these should be impossible."
    [{k:k', v}] -> Singleton h k' v
    rest -> Collision h rest
deleteImpl k hash' s n@(Bitmapped 0 a) = n
deleteImpl k hash' s n@(Bitmapped bm a) =
  let bit = 1 `shl` ((hash' `zshr` s) .&. 31)
  in if bm .&. bit == 0 then n -- not found
     else let index = popCount (bm .&. (bit-1))
              arity = A.length a
          in case deleteImpl k hash' (s+5) (unsafeArrayIndex a index) of
            -- TODO this is a bit of a mess. Check Steindorfer & Vinju
            -- again for possibly better ordering of conditions.
            Bitmapped 0 _ ->
              if arity == 1
              then empty
              else if arity == 2 && isPrim (unsafeArrayIndex a (1 - index))
                   then unsafeArrayIndex a (1 - index)
                   else Bitmapped (bm .&. complement bit) (unsafeDeleteAt index a)
            single@Singleton _ _ _ ->
              if arity == 1
              then single
              else Bitmapped bm (unsafeUpdateAt index single a)
            coll@Collision _ _ ->
              if arity == 1
              then coll
              else Bitmapped bm (unsafeUpdateAt index coll a)
            r -> Bitmapped bm (unsafeUpdateAt index r a)

-- helper for deleteImpl
isPrim :: forall k v. HashMap k v -> Boolean
isPrim (Singleton _ _ _) = true
isPrim (Collision _ _) = true
isPrim (Bitmapped _ _) = false


-- | Insert the value, delete a value, or update a value for a key in a map
alter :: forall k v. Hashable k => (Maybe v -> Maybe v) -> k -> HashMap k v -> HashMap k v
alter f k m = case f (lookup k m) of
  Nothing -> delete k m
  Just v -> insert k v m

-- | Update or delete the value for a key in a map
update :: forall k v. Hashable k => (v -> Maybe v) -> k -> HashMap k v -> HashMap k v
update f k m = alter (maybe Nothing f) k m

-- | Union two maps, same as `Semigroup.append` aka `(<>)`
-- |
-- | For duplicate keys, we keep the value from the left map.
union :: forall k v. Hashable k => HashMap k v -> HashMap k v -> HashMap k v
union (Bitmapped 0 _) r = r
union l (Bitmapped 0 _) = l
union (Singleton h k v) r = insertImpl r k v h 0
-- TODO there are more cases where we could be clever, merging bitmaps
-- and so forth for now, just fall back to iterative insertion
union l r = foldl (\m (Tuple k v) -> insert k v m) r (toUnfoldableUnordered l :: List (Tuple k v))

size :: forall k v. HashMap k v -> Int
size (Singleton _ _ _) = 1
size (Collision _ l) = A.length l
size (Bitmapped _ a) = foldl (\s m -> size m + s) 0 a
