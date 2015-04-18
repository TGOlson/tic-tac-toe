module Board where

import Cell
import Symbol
import Utils

import Data.Array
import Data.List

data Board = Board (Array Int Symbol)

data Move = Move Cell

instance Show Board where
  show (Board a) = showBoard $ elems a


makeBoard :: [Symbol] -> Board
makeBoard xs = Board (listArray (0, 8) xs)


makeEmptyBoard :: Board
makeEmptyBoard = makeBoard $ replicate 9 E


showBoard :: [Symbol] -> String
showBoard = intercalate "\n-----\n" . map showRow . groupsOf 3

showRow :: [Symbol] -> String
showRow = intercalate "|" . map show

makeMove :: Move -> Board -> Board
makeMove (Move cell) (Board a) = Board $ (//) a [cell]

getOpenCells :: Board -> [Cell]
getOpenCells (Board a) = filter isEmpty $ assocs a

getOpenCellNumbers :: Board -> [Int]
getOpenCellNumbers = map fst . getOpenCells

isTerminal :: Board -> Bool
isTerminal = null . getOpenCells
