module Cell where

import Marker


type Cell = (Int, Marker)


makeEmptyCell :: Int -> Cell
makeEmptyCell i = (i, Marker Nothing)


makeEmptyCells :: [Int] -> [Cell]
makeEmptyCells = map makeEmptyCell


isEmptyCell :: Cell -> Bool
isEmptyCell = isEmptyMarker . snd


setMarker :: Marker -> Cell -> Cell
setMarker marker (i, _) = (i, marker)
