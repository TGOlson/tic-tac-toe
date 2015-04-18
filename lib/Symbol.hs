module Symbol where

data Symbol = E | X | O deriving (Eq)

instance Show Symbol where
  show E = " "
  show X = "X"
  show O = "O"

toggle :: Symbol -> Symbol
toggle E = E
toggle X = O
toggle O = X
