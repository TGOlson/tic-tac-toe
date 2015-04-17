module Utils where

groupsOf :: Int -> [a] -> [[a]]
groupsOf n xs
  | n >= length xs = [xs]
  | otherwise = take n xs : groupsOf n (drop n xs)
