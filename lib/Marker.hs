module Marker where

import Symbol

import Data.Maybe

data Marker = Marker (Maybe Symbol)


instance Show Marker where
  show (Marker Nothing) = " "
  show marker = show $ getSymbol marker


makeMarkerWithSymbol :: Symbol -> Marker
makeMarkerWithSymbol symbol = Marker (Just symbol)


getSymbol :: Marker -> Symbol
getSymbol (Marker Nothing) = error "Cannot get symbol of empty marker"
getSymbol (Marker (Just s)) = s


isEmptyMarker :: Marker -> Bool
isEmptyMarker (Marker a) = isNothing a
