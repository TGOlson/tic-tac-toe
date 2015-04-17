module Board where

import Symbol
import Utils
import Move

import Data.Array
import Data.List

data Board = Board (Array Int Symbol)

instance Show Board where
  show = showBoard


board :: [Symbol] -> Board
board xs = Board (listArray (0, 8) xs)

showBoard :: Board -> String
showBoard (Board a) = intercalate "\n" . map show . groupsOf 3 $ elems a


emptyBoard :: Board
emptyBoard = board $ replicate 9 E


makeMove :: Move -> Board -> Board
makeMove (Move s n) (Board a) = Board $ (//) a [(n, s)]

openMoves :: Board -> [(Int, Symbol)]
openMoves (Board a) = filter ((== E) . snd) $ assocs a


isTerminal :: Board -> Bool
isTerminal = (==0) . length . openMoves
