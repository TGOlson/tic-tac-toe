import Board.Board

import Symbol
import Marker
import Cell
import Utils.Random

import Data.Array()


play :: Symbol -> Board -> IO Board
play symbol board = do
    _ <- printBoard board
    if isFull board then return board
      else do
        let nextSymbol = next symbol
        nextMove <- getNextMove nextSymbol board
        let nextBoard = makeMove nextMove board
        play nextSymbol nextBoard


printBoard :: Board -> IO ()
printBoard b = do
  print b
  putStrLn ""


getAIMove :: Symbol -> Board -> IO Cell
getAIMove symbol b = do
  cell <- randomElem (getOpenCells b)
  return $ setMarker (Marker (Just symbol)) cell


getPlayerMove :: Symbol -> Board -> IO Cell
getPlayerMove symbol _ = do
  coords <- getLine
  return (read coords, Marker (Just symbol))


getNextMove :: Symbol -> Board -> IO Cell
-- getNextMove X = getPlayerMove X
getNextMove X = getAIMove X
getNextMove O = getAIMove O

main :: IO Board
main = play X $ makeBoard 9
