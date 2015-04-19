module Cell where

import Marker
import Symbol


type Cell = (Int, Marker)


makeCell :: Marker -> Int -> Cell
makeCell marker i = (i, marker)


makeEmptyCell :: Int -> Cell
makeEmptyCell = makeCell (Marker Nothing)


makeEmptyCells :: [Int] -> [Cell]
makeEmptyCells = map makeEmptyCell


isEmptyCell :: Cell -> Bool
isEmptyCell = isEmptyMarker . snd


setMarker :: Marker -> Cell -> Cell
setMarker marker (i, _) = (i, marker)


setCellMarkerSymbol :: Symbol -> Cell -> Cell
setCellMarkerSymbol = setMarker . makeMarkerWithSymbol
