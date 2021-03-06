{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Math
  ( Vector,
    fix,
    powerset,
    powerset',
    distance,
    toVector,
    centroid,
    kMeans,
    initializeSimple,
    permute,
    cartesianProduct,
  )
where

import Control.Monad (filterM)
import Data.List
import qualified Data.Map as M

class Ord v => Vector v where
  distance :: v -> v -> Double
  centroid :: [v] -> v

class Vector v => Vectorizable e v where
  toVector :: e -> v

instance Vectorizable (Double, Double) (Double, Double) where
  toVector = id

instance Vector (Double, Double) where
  distance (a, b) (c, d) = sqrt $ (a - c) * (a - c) + (b - d) * (b - d)
  centroid lst =
    let (u, v) = foldr (\(a, b) (c, d) -> (a + c, b + d)) (0, 0) lst
        n = fromIntegral $ length lst
     in (u / n, v / n)

clusterAssignmentPhase :: (Ord v, Vector v, Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignmentPhase centroids points =
  let initialMap = M.fromList $ zip centroids (repeat [])
   in foldr
        ( \p m ->
            let chosenC = minimumBy (compareDistance p) centroids
             in M.adjust (p :) chosenC m
        )
        initialMap
        points
  where
    compareDistance p x y = compare (distance x $ toVector p) (distance y $ toVector p)

newCentroidPhase :: (Vector v, Vectorizable e v) => M.Map v [e] -> [(v, v)]
newCentroidPhase = M.toList . fmap (centroid . map toVector)

shouldStop :: (Vector v) => [(v, v)] -> Double -> Bool
shouldStop centroids threshold =
  foldr (\(x, y) s -> s + distance x y) 0.0 centroids < threshold

kMeans ::
  (Vector v, Vectorizable e v) =>
  (Int -> [e] -> [v]) ->
  Int ->
  [e] ->
  Double ->
  [v]
kMeans i k points = kMeans' (i k points) points

kMeans' ::
  (Vector v, Vectorizable e v) =>
  [v] ->
  [e] ->
  Double ->
  [v]
kMeans' centroids points threshold =
  let assignments = clusterAssignmentPhase centroids points
      oldNewCentroids = newCentroidPhase assignments
      newCentroids = map snd oldNewCentroids
   in if shouldStop oldNewCentroids threshold
        then newCentroids
        else kMeans' newCentroids points threshold

initializeSimple :: Int -> [e] -> [(Double, Double)]
initializeSimple 0 _ = []
initializeSimple n v =
  (fromIntegral n, fromIntegral n) :
  initializeSimple (n -1) v

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x : xs) = existing ++ map (x :) existing
  where
    existing = powerset xs

-- This is a somewhat mind-blowing definition of powerset
-- See discussion: https://stackoverflow.com/questions/25476248/powerset-function-1-liner
powerset' :: [a] -> [[a]]
powerset' = filterM (const [False, True])

-- Returns the "fix point" of a function, i.e. a value x such that
-- x == f x
fix :: Eq a => (a -> a) -> a -> a
fix f x = if x == x' then x else fix f x'
  where
    x' = f x

-- Same as Data.List.permutations
permute :: Eq a => [a] -> [[a]]
permute [] = [[]]
permute xs = [x : others | x <- xs, others <- permute (filter (/= x) xs)]

-- Cartesian product of an arbitrary collection of lists
cartesianProduct :: [[a]] -> [[a]]
cartesianProduct [] = [[]]
cartesianProduct (xs : xss) = [x : ys | x <- xs, ys <- cartesianProduct xss]
