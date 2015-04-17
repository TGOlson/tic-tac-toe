import Board
import Symbol

import Data.Array


play :: Symbol -> Board -> IO Board
play s board
  | isTerminal board = printBoard board
  | otherwise = do
      printBoard board
      let nextSymbol = next s
      play nextSymbol (getNextBoard nextSymbol board)


printBoard :: Board -> IO Board
printBoard b = do
  print b
  putStrLn ""
  return b


getNextBoard :: Symbol -> Board -> Board
getNextBoard s board = makeMove (length (openMoves board) - 1) s board


main = play X emptyBoard
