import Board
import Symbol
import Cell
import Random

import Data.Array()


play :: Symbol -> Board -> IO Board
play s b = do
    _ <- printBoard b
    if isTerminal b then return b
      else do
        let nextSymbol = toggle s
        nextMove <- getNextMove nextSymbol b
        let nextBoard = makeMove nextMove b
        play nextSymbol nextBoard


printBoard :: Board -> IO ()
printBoard b = do
  print b
  putStrLn ""


getAIMove :: Symbol -> Board -> IO Cell
getAIMove symbol b = do
  cellNum <- randomElem (getOpenCellNumbers b)
  return (cellNum, symbol)


getPlayerMove :: Symbol -> Board -> IO Cell
getPlayerMove symbol _ = do
  coords <- getLine
  return (read coords, symbol)


getNextMove :: Symbol -> Board -> IO Cell
getNextMove E = error "Cannot make empty move."
-- getNextMove X = getPlayerMove X
getNextMove X = getAIMove X
getNextMove O = getAIMove O

main :: IO Board
main = play X makeEmptyBoard
