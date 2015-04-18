module Symbol where

data Symbol = Empty | X | O deriving (Eq)

instance Show Symbol where
  show Empty = " "
  show X     = "X"
  show O     = "O"


toggle :: Symbol -> Symbol
toggle Empty = Empty
toggle X     = O
toggle O     = X
