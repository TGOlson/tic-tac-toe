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


getAIMove :: Board -> IO Cell
getAIMove b = randomElem (getOpenCells b)


getPlayerMove :: Board -> IO Cell
getPlayerMove _ = do
  coords <- getLine
  return (read coords, Marker Nothing)


getNextMove :: Symbol -> Board -> IO Cell
getNextMove symbol b = do
  cell <- getAIMove b
  return $ setCellMarkerSymbol symbol cell


main :: IO Board
main = play X $ makeBoard 9
