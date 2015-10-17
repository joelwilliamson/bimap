module Test.Tests where

import Data.List (nub, sort)
import qualified Data.Set as S
import Prelude hiding (null, lookup, filter,map)
import qualified Prelude as P
import Test.QuickCheck
import Control.Applicative((<$>))

import Data.Bimap


(.:) = (.).(.)

instance (Ord a, Arbitrary a, Ord b, Arbitrary b)
    => Arbitrary (Bimap a b) where
    arbitrary = fromList `fmap` arbitrary

instance (Ord a, CoArbitrary a, Ord b, CoArbitrary b)
    => CoArbitrary (Bimap a b) where
    coarbitrary = coarbitrary . toList

-- generator for filter/partition classification functions
data FilterFunc a b = FilterFunc String (a -> b -> Bool)
instance Show (FilterFunc a b) where
    show (FilterFunc desc _) = desc
instance (Integral a, Arbitrary a, Integral b, Arbitrary b)
    => Arbitrary (FilterFunc a b) where
    arbitrary = do
        pivot <- (arbitrary :: Gen Integer)
        return $ FilterFunc
            ("(\\x y -> x - y < " ++ show pivot ++ ")")
            (\x y -> fromIntegral x - fromIntegral y < pivot)
instance (Integral a, CoArbitrary a, Integral b, CoArbitrary b)
    => CoArbitrary (FilterFunc a b) where
    coarbitrary _ gen = do
        x <- arbitrary
        coarbitrary (x :: Int) gen


-- empty bimap has zero size
prop_size_empty = size empty == 0

-- empty bimap is null
prop_null_empty = null empty

-- when converting from a list and back, each pair in the latter
-- list was originally in the former list
-- (heh, this is probably made redundant by polymorphism)
prop_fromList_toList xs =
    let xs' = toList . fromList $ xs
    in all (flip elem xs) xs'
    where
    _ = xs :: [(Int, Integer)]

-- when converting a list to a bimap, each list element either
-- ends up in the bimap, or could conceivably have been clobbered
prop_fromList_account xs = all (\x -> isMember x || notUnique x) xs
    where
    _ = xs :: [(Int, Integer)]
    bi = fromList xs
    isMember x = x `pairMember` bi
    notUnique (x, y) = 
        ((>1) . length . P.filter (== x) . P.map fst $ xs) ||
        ((>1) . length . P.filter (== y) . P.map snd $ xs)

-- a bimap created from a list is no larger than the list
prop_fromList_size xs = (size $ fromList xs) <= length xs
    where
    _ = xs :: [(Int, Integer)]

-- a monotone bimap can be reconstituted via fromAscPairList
prop_fromAscPairList_reconstitute xs = and
    [ valid bi'
    , (bi == bi')
    ]
    where
    xs' = zip (sort $ P.map fst xs) (sort $ P.map snd xs)
    bi :: Bimap Int Integer
    bi = fromList xs'
    bi' = fromAscPairList . toAscList $ bi

-- fromAscPairList will never produce an invalid bimap
prop_fromAscPairList_check xs = valid bi
    where
    xs' = zip (nub $ sort $ P.map fst xs) (nub $ sort $ P.map snd xs)
    bi :: Bimap Int Integer
    bi = fromAscPairList xs'

-- if a pair is a member of the bimap, then both elements are present
-- and associated with each other
prop_pairMember bi k v =
    ((k, v) `pairMember` bi) == and
        [ k `member`  bi
        , v `memberR` bi
        , lookup  k bi == Just v
        , lookupR v bi == Just k
        ]
    where
    _ = bi :: Bimap Int Integer

-- an inserted pair ends up in the bimap
prop_insert_member bi k v = (k, v) `pairMember` (insert k v bi)
    where
    _ = bi :: Bimap Int Integer

-- if we insert a pair with an existing value, the old value's twin
-- is no longer in the bimap
prop_clobberL bi b' =
    (not . null $ bi) && (b' `notMemberR` bi)
    ==>
    (a, b) `pairNotMember` insert a b' bi
    where
    (a, b) = head . toList $ bi :: (Int, Integer)

prop_clobberR bi a' =
    (not . null $ bi) && (a' `notMember` bi)
    ==>
    (a, b) `pairNotMember` insert a' b bi
    where
    (a, b) = head . toList $ bi :: (Int, Integer)

-- if we politely insert two members, neither of which is present,
-- then the two are successfully associated
prop_tryInsert_member bi k v = (k, v) `neitherMember` bi ==>
    pairMember (k, v) (tryInsert k v bi)
    where
    _ = bi :: Bimap Int Integer
    neitherMember (k, v) bi = k `notMember` bi && v `notMemberR` bi

-- polite insertion will never remove existing associations
prop_tryInsert_not_clobber bi k v =
    all (flip pairMember $ tryInsert k v bi) (toList bi)
    where
    _ = bi :: Bimap Int Integer

-- an arbitrary bimap is valid
prop_valid bi = valid bi
    where
    _ = bi :: Bimap Int Integer

-- if x maps to y, then y maps to x
prop_member_twin bi = flip all (toList bi) $ \(x, y) -> and
    [ (bi !  x) `memberR` bi
    , (bi !> y) `member`  bi
    ]
    where
    _ = bi :: Bimap Int Integer

-- deleting an element removes it from the map
prop_delete bi = flip all (toList bi) $ \(x, y) -> and
    [ x `notMember`  delete  x bi
    , y `notMemberR` deleteR y bi
    ]
    where
    _ = bi :: Bimap Int Integer

-- deleting an element removes its twin from the map
prop_delete_twin bi = flip all (toList bi) $ \(x, y) -> and
    [ (bi !  x) `notMemberR` delete  x bi
    , (bi !> y) `notMember`  deleteR y bi
    ]
    where
    _ = bi :: Bimap Int Integer

-- adjust and fmap are similar
prop_adjust_fmap bi a = l === r
  where
  l = lookup a $ adjust f a bi :: Maybe Integer
  r = f <$> lookup a bi
  _ = bi :: Bimap Int Integer
  f = (1-)

prop_adjustR_fmap bi b = l == r
  where
  l = lookupR b $ adjustR f b bi :: Maybe Int
  r = f <$> lookupR b bi
  _ = bi :: Bimap Int Integer
  f = (3*)

-- a singleton bimap is valid, has one association, and the two
-- given values map to each other
prop_singleton x y = let bi = singleton x y in and
    [ valid bi
    , (x, y) `pairMember` bi
    , (bi !  x) == y
    , (bi !> y) == x
    , size bi == 1
    ]
    where
    _ = (x, y) :: (Int, Integer)

-- an always-true filter makes no changes
prop_filter_true bi =
    bi == filter (curry $ const True) bi
    where
    _ = bi :: Bimap Int Integer

-- an always-false filter gives an empty result
prop_filter_false bi =
    null $ filter (curry $ const False) bi
    where
    _ = bi :: Bimap Int Integer

-- all elements of the projection satisfy the predicate, and all
-- elements of the rejection do not
prop_partition_agree bi (FilterFunc _ ff) = and
    [ all (      uncurry ff) (toList projection)
    , all (not . uncurry ff) (toList rejection)
    ]
    where
    _ = bi :: Bimap Int Integer
    (projection, rejection) = partition ff bi

-- the two halves of a partition are disjoint
prop_partition_disjoint bi (FilterFunc _ ff) =
    S.null $ S.intersection (asSet projection) (asSet rejection)
    where
    _ = bi :: Bimap Int Integer
    (projection, rejection) = partition ff bi
    asSet = S.fromList . toList

-- the two halves of a partition contain the elements of the original
-- bimap
prop_partition_union bi (FilterFunc _ ff) =
    (==) (asSet bi) $
        S.union (asSet projection) (asSet rejection)
    where
    _ = bi :: Bimap Int Integer
    (projection, rejection) = partition ff bi
    asSet = S.fromList . toList

-- the two halves of a partition agree with individual filters
prop_partition_filter bi (FilterFunc _ ff) = and
    [ projection == filter (       ff) bi
    , rejection  == filter (not .: ff) bi
    ]
    where
    _ = bi :: Bimap Int Integer
    (projection, rejection) = partition ff bi

-- partition and filter produce valid results
prop_partition_filter_valid bi (FilterFunc _ ff) = all valid
    [ projection
    , rejection
    , filter (       ff) bi
    , filter (not .: ff) bi
    ]
    where
    _ = bi :: Bimap Int Integer
    (projection, rejection) = partition ff bi

-- twist is its own inverse
prop_twist_twist bi =
    bi == (twist . twist $ bi)
    where
    _ = bi :: Bimap Int Integer

-- the property (fromList == fromAList . reverse) only holds
-- if either the left or right values are all distinct
prop_fromList_fromAList xs = and
    [ fromList  ys == fromAList rys
    , fromList rys == fromAList  ys
    ]
    where
    ys = xs `zip` [1..] :: [(Int, Integer)]
    rys = reverse ys

swap (x, y) = (y, x)

-- deleteFindMin and deleteMin agree
prop_deleteMin_is_delete bi = not (null bi) ==>
    snd (deleteFindMin bi) == deleteMin bi
    where
    _ = bi :: Bimap Int Integer

-- deleteFindMin and findMin agree
prop_deleteMin_is_find bi = not (null bi) ==>
    fst (deleteFindMin bi) == findMin bi
    where
    _ = bi :: Bimap Int Integer

-- elements removed by deleteFindMin are no longer in the bimap
prop_deleteMin_deletes bi = not (null bi) ==>
    fst (deleteFindMin bi) `pairNotMember` snd (deleteFindMin bi)
    where
    _ = bi :: Bimap Int Integer

-- findMin finds a member of the map
prop_findMin_member bi = not (null bi) ==>
    findMin bi `pairMember` bi
    where
    _ = bi :: Bimap Int Integer

-- the minimum of a singleton bimap is its contents
prop_singleton_is_findMin x y = findMin bi == (x, y)
    where
    bi :: Bimap Int Integer
    bi = singleton x y

-- deleting the minimum of a singleton leaves it empty
prop_singleton_deleteMin_empty x y = null (deleteMin bi)
    where
    bi :: Bimap Int Integer
    bi = singleton x y

-- the minimum of a bimap is <= all other elements
prop_findMin_is_minimal bi = all (\ (a, _) -> a >= x) lst
    where
    lst = toList bi
    _ = bi :: Bimap Int Integer
    x = fst . findMin $ bi

prop_deleteMinR_is_delete bi = not (null bi) ==>
    snd (deleteFindMinR bi) == deleteMinR bi 
    where
    _ = bi :: Bimap Int Integer

prop_deleteMinR_is_find bi = not (null bi) ==>
    fst (deleteFindMinR bi) == findMinR bi 
    where
    _ = bi :: Bimap Int Integer

prop_deleteMinR_deletes bi = not (null bi) ==>
    (swap . fst) (deleteFindMinR bi) `pairNotMember` snd (deleteFindMinR bi)
    where
    _ = bi :: Bimap Int Integer

prop_findMinR_member bi = not (null bi) ==>
    swap (findMinR bi) `pairMember` bi
    where
    _ = bi :: Bimap Int Integer
        
prop_singleton_is_findMinR x y = findMinR bi == (y, x)
    where
    bi :: Bimap Int Integer
    bi = singleton x y

prop_singleton_deleteMinR_empty x y = null (deleteMinR bi)
    where
    bi :: Bimap Int Integer
    bi = singleton x y

prop_findMinR_is_minimal bi = all (\ (_, b) -> b >= y) lst
    where
    lst = toList bi
    _ = bi :: Bimap Int Integer
    y = fst . findMinR $ bi

prop_deleteMax_is_delete bi = not (null bi) ==>
    snd (deleteFindMax bi) == deleteMax bi
    where
    _ = bi :: Bimap Int Integer

prop_deleteMax_is_find bi = not (null bi) ==>
    fst (deleteFindMax bi) == findMax bi
    where
    _ = bi :: Bimap Int Integer

prop_deleteMax_deletes bi = not (null bi) ==>
    fst (deleteFindMax bi) `pairNotMember` snd (deleteFindMax bi)
    where
    _ = bi :: Bimap Int Integer

prop_findMax_member bi = not (null bi) ==>
    findMax bi `pairMember` bi
    where
    _ = bi :: Bimap Int Integer
        
prop_singleton_is_findMax x y = findMax bi == (x, y)
    where
    bi :: Bimap Int Integer
    bi = singleton x y

prop_singleton_deleteMax_empty x y = null (deleteMax bi)
    where
    bi :: Bimap Int Integer
    bi = singleton x y

prop_findMax_is_maximal bi = all (\ (a, _) -> a <= x) lst
    where
    lst = toList bi
    _ = bi :: Bimap Int Integer
    x = fst . findMax $ bi

prop_deleteMaxR_is_delete bi = not (null bi) ==>
    snd (deleteFindMaxR bi) == deleteMaxR bi 
    where
    _ = bi :: Bimap Int Integer

prop_deleteMaxR_is_find bi = not (null bi) ==>
    fst (deleteFindMaxR bi) == findMaxR bi
    where
    _ = bi :: Bimap Int Integer

prop_deleteMaxR_deletes bi = not (null bi) ==>
    (swap . fst) (deleteFindMaxR bi) `pairNotMember` snd (deleteFindMaxR bi)
    where
    _ = bi :: Bimap Int Integer

prop_findMaxR_member bi = not (null bi) ==>
    swap (findMaxR bi) `pairMember` bi
    where
    _ = bi :: Bimap Int Integer
        
prop_singleton_is_findMaxR x y = findMaxR bi == (y, x)
    where
    bi :: Bimap Int Integer
    bi = singleton x y

prop_singleton_deleteMaxR_empty x y = null (deleteMaxR bi)
    where
    bi :: Bimap Int Integer
    bi = singleton x y

prop_findMaxR_is_maximal bi = all (\ (_, b) -> b <= y) lst
    where
    lst = toList bi
    _ = bi :: Bimap Int Integer
    y = fst . findMaxR $ bi

prop_deleteMin_is_valid bi = not (null bi) ==>
    valid (deleteMin bi)
    where
    _ = bi :: Bimap Int Integer

prop_deleteFindMin_is_valid bi = not (null bi) ==>
    valid (snd $ deleteFindMin bi)
    where
    _ = bi :: Bimap Int Integer

prop_deleteMinR_is_valid bi = not (null bi) ==>
    valid (deleteMinR bi)
    where
    _ = bi :: Bimap Int Integer

prop_deleteFindMinR_is_valid bi = not (null bi) ==>
    valid (snd $ deleteFindMinR bi)
    where
    _ = bi :: Bimap Int Integer

prop_deleteMax_is_valid bi = not (null bi) ==>
    valid (deleteMax bi)
    where
    _ = bi :: Bimap Int Integer

prop_deleteFindMax_is_valid bi = not (null bi) ==>
    valid (snd $ deleteFindMax bi)
    where
    _ = bi :: Bimap Int Integer

prop_deleteMaxR_is_valid bi = not (null bi) ==>
    valid (deleteMaxR bi)
    where
    _ = bi :: Bimap Int Integer

prop_deleteFindMaxR_is_valid bi = not (null bi) ==>
    valid (snd $ deleteFindMaxR bi)
    where
    _ = bi :: Bimap Int Integer

prop_map_preserve_keys bi =
    (Data.List.sort $ P.map f $ keys bi) == (keys $ map f bi)
    where
    f = (4/) -- This is an arbitrary function
    _ = bi :: Bimap Double Integer

prop_map_preserve_lookup bi v =
    (lookup (f v) $ map f bi) == (lookup v bi :: Maybe Integer)
    where
    f = (1-)
    _ = bi :: Bimap Int Integer

prop_map_preserve_right_keys bi =
    (Data.List.sort $ P.map f $ keysR bi) == (keysR $ mapR f bi)
    where
    f = (4/) -- This is an arbitrary function
    _ = bi :: Bimap Int Double

prop_map_preserve_lookupR bi v =
    (lookup v $ mapR f bi) == (f <$> lookup v bi :: Maybe Integer)
    where
    f = (1-)
    _ = bi :: Bimap Int Integer

prop_mapMonotonic_preserve_keys bi =
    (P.map f $ keys bi) == (keys $ mapMonotonic f bi)
    where
    f = (3+) -- This is an arbitrary monotonic function
    _ = bi :: Bimap Double Integer

prop_mapMonotonic_preserve_lookup bi v =
    (lookup (f v) $ mapMonotonic f bi) == (lookup v bi :: Maybe Integer)
    where
    f = (2*)
    _ = bi :: Bimap Int Integer

prop_mapMontonic_preserve_right_keys bi =
    (P.map f $ keysR bi) == (keysR $ mapMonotonicR f bi)
    where
    f = (^2) -- This is an arbitrary monotonic function
    _ = bi :: Bimap Int Double

prop_mapMonotonic_preserve_lookupR bi v =
    (lookup v $ mapMonotonicR f bi) == (f <$> lookup v bi :: Maybe Integer)
    where
    f = (1-)
    _ = bi :: Bimap Int Integer
