module Board.Display where

import Marker
import Utils.List

import Data.List


rowDelimiter :: String
rowDelimiter = "\n-----\n"


cellDelimiter :: String
cellDelimiter = "|"


showBoard :: [Marker] -> String
showBoard = intercalate rowDelimiter . map showRow . groupsOf 3


showRow :: [Marker] -> String
showRow = intercalate cellDelimiter . map show
