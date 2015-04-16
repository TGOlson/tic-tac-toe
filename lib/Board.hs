module Board where

import Symbol

import Data.Array
import Data.List

data Board = Board (Array Int Symbol)

instance Show Board where
  show = showBoard

board :: [Symbol] -> Board
board xs = Board (listArray (0, 8) xs)


showBoard :: Board -> String
showBoard (Board a) = concat . intersperse "\n" . map show . groupsOf 3 $ elems a

groupsOf :: Int -> [a] -> [[a]]
groupsOf n xs
  | n >= length xs = [xs]
  | otherwise = take n xs : groupsOf n (drop n xs)

emptyBoard :: Board
emptyBoard = board $ replicate 9 E
