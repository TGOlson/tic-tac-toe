module Cell where

import Symbol

type Cell = (Int, Symbol)

isEmptyCell :: Cell -> Bool
isEmptyCell (_, Empty) = True
isEmptyCell _ = False
