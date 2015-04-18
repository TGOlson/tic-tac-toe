import Board
import Symbol

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


getAIMove :: Symbol -> Board -> Move
getAIMove symbol b = Move (length (openMoves b) - 1, symbol)


getPlayerMove :: Symbol -> Board -> IO Move
getPlayerMove symbol _ = do
  coords <- getLine
  return $ Move (read coords, symbol)


getNextMove :: Symbol -> Board -> IO Move
getNextMove E = error "Cannot make empty move."
-- getNextMove X = getPlayerMove X
getNextMove X = return . getAIMove X
getNextMove O = return . getAIMove O

main :: IO Board
main = play X makeEmptyBoard
