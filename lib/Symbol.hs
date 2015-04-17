module Symbol where

data Symbol = E | X | O deriving (Show, Eq, Enum)

next :: Symbol -> Symbol
next E = E
next X = O
next O = X
