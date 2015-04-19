module Utils.List where

import Utils.Int

import Data.List


allEq :: Eq a => [a] -> Bool
allEq xs = all (== head xs) (tail xs)


groupsOf :: Int -> [a] -> [[a]]
groupsOf n xs
  | n >= length xs = [xs]
  | otherwise = take n xs : groupsOf n (drop n xs)


isSquareList :: [a] -> Bool
isSquareList = isSquare . length


rows :: [a] -> [[a]]
rows xs
  | isSquareList xs = groupsOf (sqrtInt $ length xs) xs
  | otherwise = error "Can only partition square lists"


cols :: [a] -> [[a]]
cols = transpose . rows


diagL :: Eq a => [a] -> [a]
diagL = diagLFromRows . rows


diagR :: Eq a => [a] -> [a]
diagR = diagLFromRows . map reverse . rows


diagLFromRows :: Eq a => [[a]] -> [a]
diagLFromRows xs = [x | i <- [0..length xs - 1], let x = xs !! i !! i]

diags :: Eq a => [a] -> [[a]]
diags xs = [diagL xs, diagR xs]


sections :: Eq a => [a] -> [[a]]
sections xs = rows xs ++ cols xs ++ diags xs
