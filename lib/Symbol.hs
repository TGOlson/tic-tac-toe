module Symbol where


data Symbol = X | O deriving (Show, Eq)


next :: Symbol -> Symbol
next X = O
next O = X


isX :: Symbol -> Bool
isX = (==) X


isO :: Symbol -> Bool
isO = (==) O
