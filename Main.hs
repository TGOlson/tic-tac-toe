import Data.List

data Symbol = E | X | O deriving (Show, Eq)

data Board = Board [Symbol] deriving (Eq)

instance Show Board where
  show = showBoard

showBoard :: Board -> String
showBoard (Board a) = concat . intersperse "\n" . map show $ groupsOf 3 a

groupsOf :: Int -> [a] -> [[a]]
groupsOf n xs
  | n >= length xs = [xs]
  | otherwise = take n xs : groupsOf n (drop n xs)


startingBoard :: Board
startingBoard = Board $ replicate 9 E


play :: Board -> IO Board
play board = do
  print board
  return board

main = play startingBoard
