import Board
import Symbol

import Data.Array()


play :: Symbol -> Board -> IO Board
play s b
  | isTerminal b = printBoard b
  | otherwise = do
      _ <- printBoard b
      let nextSymbol = next s
      play nextSymbol (getNextBoard nextSymbol b)


printBoard :: Board -> IO Board
printBoard b = do
  print b
  putStrLn ""
  return b


getNextBoard :: Symbol -> Board -> Board
getNextBoard s b = makeMove (length (openMoves b) - 1) s b

main :: IO Board
main = play X emptyBoard
