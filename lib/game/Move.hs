module Game.Move where

import Board.Board
import Cell
import Symbol

import Utils.Random


isPlayer :: Symbol -> Bool
isPlayer = isX


getAIMove :: Board -> IO Cell
getAIMove b = randomElem (getOpenCells b)


getPlayerMove :: Board -> IO Cell
getPlayerMove board = do
  putStrLn "Select a cell (1-9):"
  coords <- getLine
  let cell = getCell (read coords - 1) board
  if isEmptyCell cell then return cell
    else do
      putStrLn "Cell is not open."
      getPlayerMove board


getMove :: Symbol -> Board -> IO Cell
getMove symbol board = do
  cell <- if isPlayer symbol then getPlayerMove board else getAIMove board
  return $ setCellMarkerSymbol symbol cell
