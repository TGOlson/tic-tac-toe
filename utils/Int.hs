module Utils.Int where


sqrtInt :: Int -> Int
sqrtInt = ceiling . sqrt . fromIntegral


isSquare :: Int -> Bool
isSquare x = x == (sqrtInt x ^ 2)
