module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array as A
import Data.CHAMP as CHAMP
import Data.Foldable (foldMap)
import Data.HashMap as HashMap
import Data.Hashable (class Hashable)
import Data.List (List(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (alaF)
import Data.NonEmpty ((:|))
import Data.Tuple (Tuple(..), fst)
import Test.QuickCheck (Result(..), quickCheck, quickCheck', (<?>), (===))
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)

data Op k v
  = Lookup k
  | Insert k v

instance arbOp :: (Arbitrary k, Arbitrary v) => Arbitrary (Op k v) where
  arbitrary = oneOf $ (Insert <$> arbitrary <*> arbitrary) :| [Lookup <$> arbitrary]

newtype CollidingInt = CollidingInt Int

derive instance eqCollidingInt :: Eq CollidingInt
derive instance ordCollidingInt :: Ord CollidingInt

instance showCollidingInt :: Show CollidingInt where
  show (CollidingInt i) = show i

instance arbitraryCollidingInt :: Arbitrary CollidingInt where
  arbitrary = CollidingInt <$> arbitrary

instance collidingIntHashable :: Hashable CollidingInt where
  hash (CollidingInt i) = i `mod` 100

prop ::
  forall k v.
  Eq v => Show v =>
  Show k => Ord k => Hashable k =>
  List (Op k v) -> Result
prop = go Map.empty HashMap.empty
  where go m hm Nil = Success
        go m hm (Cons (Lookup k) rest) =
          let res = Map.lookup k m === HashMap.lookup k hm
          in case res of
            Success -> go m hm rest
            _ -> res
        go m hm (Cons (Insert k v) rest) =
          go (Map.insert k v m) (HashMap.insert k v hm) rest

arbitraryMap :: forall k v. Hashable k => Array (Tuple k v) -> HashMap.HashMap k v
arbitraryMap = HashMap.fromFoldable

arbitraryCHAMP :: forall k v. Hashable k => Array (Tuple k v) -> CHAMP.CHAMP k v
arbitraryCHAMP = CHAMP.fromFoldable


nowGood :: forall a e. Eq a => a -> a -> Eff (console :: CONSOLE, exception :: EXCEPTION | e) Unit
nowGood a b = if a == b then log "Fixed \\o/" else throw "still broken"

nowGood' :: forall e. Boolean -> Eff (console :: CONSOLE, exception :: EXCEPTION | e) Unit
nowGood' b = if b then log "Fixed \\o/" else throw "still broken"

main :: forall e. Eff (exception :: EXCEPTION, random :: RANDOM, console :: CONSOLE | e) Unit
main = do
  log "Insert & lookup like map"
  quickCheck (prop :: List (Op Boolean Int) -> Result)
  quickCheck (prop :: List (Op Int Int) -> Result)
  quickCheck (prop :: List (Op CollidingInt CollidingInt) -> Result)

  log "Insert and lookup"
  quickCheck $ \(k :: Int) (v :: Int) a ->
    HashMap.lookup k (HashMap.insert k v (arbitraryMap a)) == Just v
    <?> ("k: " <> show k <> ", v: " <> show v <> ", m: " <> show (arbitraryMap a))

  log "CHAMP Insert and lookup"
  quickCheck' 10000 $ \(k :: Int) (v :: Int) a ->
    CHAMP.lookup k (CHAMP.insert k v (arbitraryCHAMP a)) == Just v
    <?> ("k: " <> show k <> ", v: " <> show v <> ", m: " <> show a <> ", r: " <> show (CHAMP.lookup k (CHAMP.insert k v (arbitraryCHAMP a))))

  log "CHAMP insert colliding and lookup"
  quickCheck' 10000 $ \(k :: CollidingInt) (v :: Int) a ->
    CHAMP.lookup k (CHAMP.insert k v (arbitraryCHAMP a)) == Just v
    <?> ("k: " <> show k <> ",\nv: " <> show v <>
         "\nm: " <> show (CHAMP.toArrayBy Tuple (arbitraryCHAMP a)) <>
         "\ni: " <> show (CHAMP.toArrayBy Tuple (CHAMP.insert k v (arbitraryCHAMP a))) <>
         "\nl: " <> show (CHAMP.lookup k (CHAMP.insert k v (arbitraryCHAMP a))))

  log "toUnfoldableUnordered"
  quickCheck $ \ (a :: Array (Tuple CollidingInt Int)) ->
    let nubA = A.nubBy (\x y -> fst x == fst y) a
        m = arbitraryMap nubA
    in A.sort (HashMap.toUnfoldableUnordered m) == A.sort nubA

  log "CHAMP toArrayBy"
  quickCheck' 10000 $ \ (a :: Array (Tuple CollidingInt Int)) ->
    let nubA = A.nubBy (\x y -> fst x == fst y) a
        m = arbitraryCHAMP nubA
    in A.sort (CHAMP.toArrayBy Tuple m) == A.sort nubA
       <?> ("expected: " <> show (A.sort nubA) <> "\ngot:     " <> show (A.sort (CHAMP.toArrayBy Tuple m)))

  log "delete removes"
  quickCheck $ \ k v (a :: Array (Tuple CollidingInt String)) ->
    Nothing == (HashMap.lookup k $ HashMap.delete k $ HashMap.insert k v $ arbitraryMap a)

  log "CHAMP delete removes"
  quickCheck' 10000 $ \ k v (a :: Array (Tuple CollidingInt Boolean)) ->
    Nothing == (CHAMP.lookup k $ CHAMP.delete k $ CHAMP.insert k v $ arbitraryCHAMP a) <?> ("k: " <> show k <>
         "\nv: " <> show v <>
         "\na: " <> show a <>
         "\nbefore: " <> show (CHAMP.insert k v (arbitraryCHAMP a)) <>
         "\nafter : " <> show (CHAMP.delete k $ CHAMP.insert k v $ arbitraryCHAMP a))

  log "delete idempotent"
  quickCheck $ \ k (a :: Array (Tuple CollidingInt String)) ->
    let m = arbitraryMap a in
    HashMap.delete k m == HashMap.delete k (HashMap.delete k m)

  log "delete preserves structure"
  quickCheck' 100000 $ \ k v (a :: Array (Tuple CollidingInt Boolean)) ->
    let m = arbitraryMap (A.filter (\t -> fst t /= k) a) in
    HashMap.delete k (HashMap.insert k v m) == m
    <?> ("k: " <> show k <>
         "\nv: " <> show v <>
         "\na: " <> show (A.filter (\t -> fst t /= k) a) <>
         "\nexpected: " <> show m <>
         "\ngot     : " <> show (HashMap.delete k (HashMap.insert k v m)) <>
         "\ninserted: " <> show (HashMap.insert k v m))

  log "CHAMP delete preserves structure"
  quickCheck' 100000 $ \ k v (a :: Array (Tuple CollidingInt Boolean)) ->
    let m = arbitraryCHAMP (A.filter (\t -> fst t /= k) a) in
    CHAMP.delete k (CHAMP.insert k v m) == m
    <?> ("k: " <> show k <>
         "\nv: " <> show v <>
         "\na: " <> show (A.filter (\t -> fst t /= k) a) <>
         "\nexpected: " <> show m <>
         "\ngot     : " <> show (CHAMP.delete k (CHAMP.insert k v m)) <>
         "\ninserted: " <> show (CHAMP.insert k v m))

  log "CHAMP delete preserves structure -- show"
  quickCheck' 100000 $ \ k v (a :: Array (Tuple CollidingInt Boolean)) ->
    let m = arbitraryCHAMP (A.filter (\t -> fst t /= k) a) in
    show (CHAMP.delete k (CHAMP.insert k v m)) == show m
    <?> ("k: " <> show k <>
         "\nv: " <> show v <>
         "\na: " <> show (A.filter (\t -> fst t /= k) a) <>
         "\nexpected: " <> show m <>
         "\ngot     : " <> show (CHAMP.delete k (CHAMP.insert k v m)) <>
         "\ninserted: " <> show (CHAMP.insert k v m))

  log "fromFoldable (a <> b) = fromFoldable a <> fromFoldable b"
  quickCheck' 1000 \ a (b :: Array (Tuple CollidingInt String)) ->
    HashMap.fromFoldable (a <> b) === HashMap.fromFoldable a <> HashMap.fromFoldable b

  log "map id = id"
  quickCheck \ (a :: Array (Tuple CollidingInt Boolean)) ->
    let m = arbitraryMap a in
    map id m === m

  log "map (f <<< g) = map f <<< map g"
  quickCheck \ (f :: Int -> Int) (g :: Int -> Int) (a :: Array (Tuple CollidingInt Int)) ->
    let m = arbitraryMap a in
    map (f <<< g) m === map f (map g m)

  log "size = alaF Additive foldMap (const 1)"
  quickCheck \ (a :: Array (Tuple CollidingInt Int)) ->
    let m = arbitraryMap a in
    HashMap.size m === alaF Additive foldMap (const 1) m

  log "Recheck previous failures:"
  nowGood (Just 85767) $ HashMap.lookup (-102839) (HashMap.insert (-102839) 85767 (HashMap.singleton (-717313) 472415))
  nowGood (Just (-533576)) $ HashMap.lookup (-635631) (HashMap.insert (-635631) (-533576) (HashMap.singleton 406135 (-940705)))
  nowGood (Just 957310) $ HashMap.lookup (-43745) (HashMap.insert (-43745) 957310 (HashMap.singleton 321662 955811))

  nowGood' t54
  nowGood' t105
  nowGood' t249

  log "Done."

t54 :: Boolean
t54 = let k = (-538828)
          v = false
          a = [(Tuple 605832 false),(Tuple 418793 false),(Tuple (-829612) false),(Tuple (-428805) true),(Tuple (-806480) true),(Tuple 637863 false),(Tuple (-616539) true),(Tuple (-650917) false),(Tuple 592866 false)]
          m = arbitraryMap a in
      HashMap.delete k (HashMap.insert k v m) == m

t105 :: Boolean
t105 = let k = (-354590)
           v = true
           a = [(Tuple (-438814) false)]
           m = arbitraryMap a in
       HashMap.delete k (HashMap.insert k v m) == m 

t249 :: Boolean
t249 = let k = 855538
           v = false
           a = [(Tuple 359452 false),(Tuple 903388 false)]
           m = arbitraryMap a in
       HashMap.delete k (HashMap.insert k v m) == m

{-

  "CHAMP insert colliding and lookup"

k: -713082,
v: -452984
m: [(Tuple 247701 751946),(Tuple 109267 3001),(Tuple 216304 663703),(Tuple -986658 982107),(Tuple -728020 644515),(Tuple 307427 433841),(Tuple 734361 269785),(Tuple -858882 -878371),(Tuple -13482 -287465)]
i: [(Tuple 247701 751946),(Tuple 109267 3001),(Tuple 216304 663703),(Tuple -986658 982107),(Tuple -728020 644515),(Tuple 307427 433841),(Tuple 734361 269785)]

k: 834588,
v: 923037
m: [(Tuple 627567 -347288),(Tuple -197621 -403264),(Tuple 703088 -899609),(Tuple 561788 223074)]
i: [(Tuple 627567 -347288),(Tuple -197621 -403264)]

-}
