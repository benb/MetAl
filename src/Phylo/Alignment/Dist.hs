{-Copyright 2011 Ben Blackburne 

    This file is part of MetAl.

    MetAl is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    MetAl is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with MetAl.  If not, see <http://www.gnu.org/licenses/>.

-}
{-# LANGUAGE FlexibleInstances #-} 
module Phylo.Alignment.Dist where
import Phylo.Alignment
import Debug.Trace
import Data.List

--
--gapEvent :: Node -> ListAlignment -> [[Maybe Split]]
--gapEvent tree (ListAlignment names seqs cols) = gapEvent' tree names cols
--
--gapEvent' :: Node -> [Name] -> [Column] -> [[Maybe Split]]
--gapEvent' (Tree l r) names cols = if (contained leftNames gapNames)  where
--                                             gapNames = map (\x -> fst x) $ filter (\x -> (snd x=='-')) $ zip names cols 
--                                             leftNames = names l
--                                             rightNames = names r
--                                             contained x y = contained' x x y
--                                             contained' full (x:[]) (y:[]) = x==y
--                                             contained' full (x:[]) (y:ys) = x==y || contained full full ys
--                                             contained' full (x:xs) (y:ys) = x==y || contained full xs (y:ys)
--
hom0Dist = labDistPerSeq diffSSP numberifyBasic
--This is faster but less generic:
--hom0Dist = zeroDist numberifyBasic
homDist = labDistPerSeq diffIn numberifyBasic
homGapDist = labDistPerSeq diffIn numberifyGap
homTreeDist t = labDistPerSeq diffIn (numberifyGapTree t)

isPermutation :: ListAlignment -> ListAlignment -> Bool
isPermutation a b | (names a) /= (names b) = False
isPermutation a b = (dropGaps a) == (dropGaps b)


--zeroDist f = labDistTrig f
                        
siteLabel :: (ListAlignment -> [[(Int)]]) -> (Int->Maybe Int) -> (ListAlignment -> [[(Int,Maybe Int)]])
siteLabel f gapHandler = fmap (map (map toLabel)) f where 
        toLabel i = if (i < 0) then
                        (i,gapHandler i)
                    else 
                        (i,Nothing)

class (Eq a, Show a) => SiteLabel a where 
  isGap :: a -> Bool

instance SiteLabel Int where
  isGap a = a<0


instance (Integral a, Show a, Eq b, Show b,Ord a) => SiteLabel (a,b) where
  isGap (a,b) = a<0

totalDistList = summariseDistList . map summariseDistList

summariseDistList :: [(Int,Int)]->(Int,Int)
summariseDistList = foldr (\(i,j) (i2,j2) -> (i+i2,j+j2)) (0,0)

mergeDistList :: [[(Int,Int)]]->[(Int,Int)]
mergeDistList xs = map (\(i,j)->(i+j,j)) $ mergeDistList' xs []

{-mergeDistList' xs [] | traceShow (map length xs) False = undefined-}
mergeDistList' [] ys = ys
mergeDistList' (x:xs) [] = mergeDistList' xs x
mergeDistList' (x:xs) ys = mergeDistList' xs $ map (\((i,j),(i2,j2))->(i+i2,j+j2)) $ zip x ys

-- |'labDist' computes the distance between two alignments after labelling
-- it takes a labelling function and two alignments
-- and returns a a tuple of (denonimator,numerator), i.e. distance is
-- snd/fst
--labDist :: (SiteLabel a) => (DiffFunction a) -> (ListAlignment -> [[(a)]]) -> ListAlignment -> ListAlignment -> (Int,Int)
--labDist numF aln1 aln2 | trace "Fast dist" False  = undefined
--labDist diff numF aln1 aln2 =  summariseDistList ans where
--                                   ans = labDistPerSeq diff numF aln1 aln2

labDistPerSeq diff numF aln1 aln2 = ans where
                                        num1 = numF aln1
                                        num2 = numF aln2
                                        ans = labDistPerSeq' diff num1 num2 [] [] []

labDistPerSeq' diff [] [] headx heady ijs = ijs
labDistPerSeq' diff (x:xs) (y:ys) headx heady ijs = labDistPerSeq' diff xs ys (x:headx) (y:heady) ans where
                                                        ans = (mergeDistList (labDistSeq diff x y xs ys (labDistSeq diff x y headx heady []))):ijs


-- | increments the tuple (final arg) with the distance between
-- two lists of labels and each of the corresponding lists-of-lists of labels
labDistSeq :: Show a =>  DiffFunction a -> [a] -> [a] -> [[a]] ->  [[a]] -> [[(Int,Int)]] -> [[(Int,Int)]]
labDistSeq f seqA seqB (seqA2:xs) (seqB2:ys) ans = (f seqA seqA2 seqB seqB2 []) : (labDistSeq f seqA seqB xs ys ans)
labDistSeq f seqA seqB [] [] ans = ans
 
traceX x = trace ("OK " ++ (show x)) x


----labDistTrig is like labDist but only does a vs b and not b vs a
--labDistTrig numF aln1 aln2 = (i+j,j) where
--                                num1 = numF aln1
--                                num2 = numF aln2
--                                (i,j) = labDistTrig' num1 num2 (0,0)
--        
--labDistTrig' ::  (SiteLabel a, Ord a) => [[(a)]] ->  [[(a)]] -> (Int,Int) -> (Int,Int)
--labDistTrig' (x:xs) (y:ys) (i,j) = i `seq` j `seq` labDistTrig' xs ys (labDistSeq diffSSP x y xs ys (i,j))
--labDistTrig' [] [] t = t 
--      
type DiffFunction a = [a] -> [a] -> [a] -> [a] -> [(Int,Int)] -> [(Int,Int)]
-- | compute distance for pairs of labels 
diffIn :: (SiteLabel a) => DiffFunction a
--diffIn (x:xs) (y:ys) ans | trace ((show x) ++ (show y) ++ (show ans)) False = undefined
--First, skip gaps on left side of xs and ys
diffIn (x1:x1s) (x2:x2s) y1s y2s ij | (isGap x1) = ij `seq` diffIn x1s x2s y1s y2s ij
diffIn x1s x2s (y1:y1s) (y2:y2s) ij | (isGap y1) = ij `seq` diffIn x1s x2s y1s y2s ij 
--Same
diffIn (x1:x1s) (x2:x2s) (y1:y1s) (y2:y2s) ij | x2==y2  = ij `seq` diffIn x1s x2s y1s y2s $ (2,0):ij
--Different
                                              | otherwise = ij `seq` diffIn x1s x2s y1s y2s $ (0,2):ij
diffIn [] [] [] [] t = t


-- | compute distance for pairs of labels for metric 0 (SSP)
diffSSP :: (SiteLabel a, Ord a) => DiffFunction a
{-diffSSP (a:x1s) (b:x2s) (c:y1s) (d:y2s) ij | trace ("diffSSP  " ++ (show a) ++ " , " ++  (show b) ++ " : " ++ " " ++ (show c) ++ " , " ++ (show d) ++ " " ++ (show (ij))) False = undefined-}
{-diffSSP [] [] [] [] ij | trace ("diffSSP  [],[] [],[] " ++ (show (ij))) False = undefined-}
diffSSP (a:x1s) (b:x2s) y1s y2s ij | isGap a  = diffSSP x1s x2s y1s y2s ij
diffSSP x1s x2s (c:y1s) (d:y2s) ij | isGap c  = diffSSP x1s x2s y1s y2s ij
diffSSP (a:x1s) (b:x2s) (c:y1s) (d:y2s) ij | a/=c = trace ((show a ) ++ " " ++ (show c)) $ error "ERROR!"
diffSSP (a:x1s) (b:x2s) (c:y1s) (d:y2s) ij | isGap b && (not $ isGap d) = diffSSP x1s x2s y1s y2s $ (0,1):ij -- we have a gap, other has pair
diffSSP (a:x1s) (b:x2s) (c:y1s) (d:y2s) ij | isGap b && isGap d = diffSSP x1s x2s y1s y2s $ (0,0):ij -- we have 2 gaps 
diffSSP (a:x1s) (b:x2s) (c:y1s) (d:y2s) ij | isGap d = diffSSP x1s x2s y1s y2s $ (0,1):ij -- other has a gap so our pair is unique
diffSSP (a:x1s) (b:x2s) (c:y1s) (d:y2s) ij | b/=d  = diffSSP x1s x2s y1s y2s $ (0,2):ij -- other has a different pair so ours is unique
diffSSP (a:x1s) (b:x2s) (c:y1s) (d:y2s) ij | b==d  = diffSSP x1s x2s y1s y2s $ (1,0):ij -- match
diffSSP [] [] [] [] ij = ij 
