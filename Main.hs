import Board

play :: Board -> IO Board
play board = do
  print board
  return board

main = play emptyBoard
