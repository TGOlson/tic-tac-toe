import Board
import Symbol
import Move


import Data.Array()


play :: Symbol -> Board -> IO Board
play s b
  | isTerminal b = printBoard b
  | otherwise = do
      _ <- printBoard b
      let nextSymbol = next s
      nextMove <- getNextMove nextSymbol b
      play nextSymbol (makeMove nextMove b)


printBoard :: Board -> IO Board
printBoard b = do
  print b
  putStrLn ""
  return b


-- data Move = Move Int

getNextMove :: Symbol -> Board -> IO Move
getNextMove E _ = error "Cannot make empty move."
getNextMove O b = return $ Move O (length (openMoves b) - 1)
getNextMove X _ = do
  coords <- getLine
  return $ Move X (read coords)

main :: IO Board
main = play X emptyBoard
