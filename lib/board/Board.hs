module Board.Board where

import Board.Display
import Cell
import Marker

import Data.Array


data Board = Board (Array Int Marker)


instance Show Board where
  show (Board a) = showBoard $ elems a


makeBoard :: Int -> Board
makeBoard i = Board (array (0, i - 1) (makeEmptyCells [0..8]))


setCell :: Cell -> Board -> Board
setCell cell (Board a) = Board $ (//) a [cell]


getCells :: Board -> [Cell]
getCells (Board a) = assocs a


getCell :: Int -> Board -> Cell
getCell i = (!! i) . getCells


getOpenCells :: Board -> [Cell]
getOpenCells = filter isEmptyCell . getCells


isFull :: Board -> Bool
isFull = null . getOpenCells
