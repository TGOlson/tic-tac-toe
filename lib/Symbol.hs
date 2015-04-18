module Symbol where


data Symbol = X | O deriving (Show)


next :: Symbol -> Symbol
next X = O
next O = X
