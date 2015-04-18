module Cell where

import Symbol

type Cell = (Int, Symbol)

isEmpty :: Cell -> Bool
isEmpty = (== E) . snd
