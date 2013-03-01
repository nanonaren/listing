Naren Sundar

\begin{code}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}

module Data.Listing
    (
      Listing (..)
    , onListing
    , translate
    ) where

import Prelude hiding (head,tail,null)
import Data.List (find)
import qualified Prelude
import qualified Data.Sequence as SQ
import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.Map.Lazy as M
import qualified Data.IntMap as IM
import qualified Data.Foldable as F

import Data.Maybe

-- | Minimal definition: toList, fromList, lookup
class Listing s where
    type Elem s
    type Index s
    toList :: s -> [Elem s]
    fromList :: [Elem s] -> s
    lookup :: s -> Index s -> Maybe (Elem s)

    singleton :: Elem s -> s
    singleton = fromList.(:[])

    size :: s -> Int
    size = length.toList

    null :: s -> Bool
    null = Prelude.null.toList

    head :: s -> Elem s
    head = Prelude.head.toList

    tail :: s -> s
    tail = onListing Prelude.tail

-- | eg: drop 5 `onListing` (V.fromList [1..10]) = V.fromList [6..10]
onListing :: Listing s => ([Elem s] -> [Elem s]) -> s -> s
onListing f = fromList.f.toList

-- | translate one structure into another
--   eg: translate [1..10] :: Maybe Int = Just 1
translate :: (Elem s ~ Elem t,Listing s,Listing t) => s -> t
translate = fromList.toList

\end{code}

Instances
---------

\begin{code}

instance Listing [a] where
    type Elem [a] = a
    type Index [a] = Int
    toList = id
    fromList = id
    -- | zero based
    lookup xs i = fmap snd.find ((==i).fst).zip [0..] $ xs

instance Listing (Maybe a) where
    type Elem (Maybe a) = a
    type Index (Maybe a) = ()
    toList = maybeToList
    fromList = listToMaybe
    lookup m _ = m
    size Nothing = 0
    size (Just _) = 1
    null Nothing = True
    null _ = False

instance Listing (SQ.Seq a) where
    type Elem (SQ.Seq a) = a
    type Index (SQ.Seq a) = Int
    toList = F.toList
    fromList = SQ.fromList
    lookup s i = if null s then Nothing else Just (SQ.index s i)
    singleton = SQ.singleton
    size = SQ.length
    null = SQ.null
    head s = let (x SQ.:< _) = SQ.viewl s
             in x
    tail s = let (_ SQ.:< t) = SQ.viewl s
             in t

instance Ord a => Listing (S.Set a) where
    type Elem (S.Set a) = a
    type Index (S.Set a) = a
    toList = S.toList
    fromList = S.fromList
    lookup s i = if S.member i s then Just i else Nothing
    singleton = S.singleton
    size = S.size
    null = S.null
    head s = let Just (x,_) = S.minView s
             in x
    tail s = let Just (_,t) = S.minView s
             in t

instance Listing IS.IntSet where
    type Elem IS.IntSet = Int
    type Index IS.IntSet = Int
    toList = IS.toList
    fromList = IS.fromList
    lookup s i = if IS.member i s then Just i else Nothing
    singleton = IS.singleton
    size = IS.size
    null = IS.null
    head s = let Just (x,_) = IS.minView s
             in x
    tail s = let Just (_,t) = IS.minView s
             in t

instance Ord k => Listing (M.Map k a) where
    type Elem (M.Map k a) = (k,a)
    type Index (M.Map k a) = k
    toList = M.toList
    fromList = M.fromList
    lookup m k = (k,) `fmap` M.lookup k m
    singleton (k,v) = M.singleton k v
    size = M.size
    null = M.null
    head s = let Just (x,_) = M.minViewWithKey s
             in x
    tail s = let Just (_,t) = M.minViewWithKey s
             in t

instance Listing (IM.IntMap a) where
    type Elem (IM.IntMap a) = (Int,a)
    type Index (IM.IntMap a) = Int
    toList = IM.toList
    fromList = IM.fromList
    lookup m k = (k,) `fmap` IM.lookup k m
    singleton (k,v) = IM.singleton k v
    size = IM.size
    null = IM.null
    head s = let Just (x,_) = IM.minViewWithKey s
             in x
    tail s = let Just (_,t) = IM.minViewWithKey s
             in t

\end{code}