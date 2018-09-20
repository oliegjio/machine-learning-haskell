module Algebra.Matrix where

data Matrix = Matrix [[Float]] deriving (Show, Eq)

(|+|) :: Matrix -> Matrix -> Matrix
(Matrix xs) |+| (Matrix ys) = Matrix $ zipWith (\ a b -> zipWith (+) a b) xs ys

--(|*|) :: Matrix -> Matrix -> Matrix
--(Matrix xs) |*| (Matrix ys) = Matrix [[ | j <- [0 .. ly]] | i <- [0 .. lx]]
--    where lx = (length xs) - 1
--          ly = (length $ transpose ys) - 1

transpose :: Matrix -> Matrix
transpose m@(Matrix xs) = Matrix [selectColumn i m | i <- [0 .. (length xs) - 1]]

selectColumn :: Int -> Matrix -> [Float]
selectColumn n (Matrix xs) = map (!! n) xs

matrixWithValues :: Int -> Int -> Float -> Matrix
matrixWithValues n m v = Matrix $ take n $ repeat $ take m $ repeat v