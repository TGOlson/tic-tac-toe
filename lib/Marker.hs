module Marker where

data Symbol' = X | O deriving (Show, Eq)

data Marker = Marker (Maybe Symbol')

getSymbol :: Marker -> Symbol'
getSymbol (Marker Nothing) = error "Cannot get symbol of empty marker"
getSymbol (Marker (Just s)) = s

isEmpty :: Marker -> Bool
isEmpty (Marker Nothing) = True
isEmpty _ = False
