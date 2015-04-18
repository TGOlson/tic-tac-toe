module Utils.Random where

import System.Random


randomElem :: [a] -> IO a
randomElem xs = do
  i <- randomNum (0, length xs - 1)
  return $ xs !! i


randomNum :: (Int, Int) -> IO Int
randomNum range = do
  gen <- newStdGen
  return . fst $ randomR range gen
