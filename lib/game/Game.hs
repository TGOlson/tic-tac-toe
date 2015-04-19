module Game.Game where

import Game.Move
import Board.Board
import Symbol


play :: Symbol -> Board -> IO Board
play symbol board = do
    _ <- printBoard board
    if isFull board then do
      putStrLn "Cats game."
      return board
      else do
        cell <- getMove symbol board
        let nextBoard = setCell cell board
        play (next symbol) nextBoard


printBoard :: Board -> IO ()
printBoard b = do
  print b
  putStrLn ""
