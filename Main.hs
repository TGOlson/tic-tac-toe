import Board
import Symbol
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


getAIMove :: Symbol -> Board -> IO Move
getAIMove symbol b = do
  cellNum <- randomElem (getOpenCellNumbers b)
  return $ Move (cellNum, symbol)


getPlayerMove :: Symbol -> Board -> IO Move
getPlayerMove symbol _ = do
  coords <- getLine
  return $ Move (read coords, symbol)


getNextMove :: Symbol -> Board -> IO Move
getNextMove E = error "Cannot make empty move."
-- getNextMove X = getPlayerMove X
getNextMove X = getAIMove X
getNextMove O = getAIMove O

main :: IO Board
main = play X makeEmptyBoard
