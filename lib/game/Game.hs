module Game.Game where

import Game.Move
import Board.Board
import Symbol
import Marker
import Utils.List


hasWinner :: Board -> Bool
hasWinner = any sectionIsClaimed . sections . getMarkers


sectionIsClaimed :: [Marker] -> Bool
sectionIsClaimed xs = allEq xs && sectionIsFull xs


play :: Symbol -> Board -> IO Board
play symbol board = do
    _ <- printBoard board
    if isFull board then do
      putStrLn "Cats game."
      return board
      else do
        cell <- getMove symbol board
        let nextBoard = setCell cell board
        if hasWinner nextBoard then do
          _ <- printBoard nextBoard
          putStr (show symbol)
          putStrLn " has won!"
          return nextBoard
          else
            play (next symbol) nextBoard


printBoard :: Board -> IO ()
printBoard b = do
  print b
  putStrLn ""
