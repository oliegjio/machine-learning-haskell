module Algebra.Matrix where

data Matrix = Matrix [[Float]] deriving (Show, Eq)

(|+|) :: Matrix -> Matrix -> Matrix
(Matrix xs) |+| (Matrix ys) = Matrix $ zipWith (\ a b -> zipWith (+) a b) xs ys

(|*|) :: Matrix -> Matrix -> Matrix
m1@(Matrix xs) |*| m2@(Matrix ys) = Matrix [row n | n <- [0 .. (length xs) - 1]]
    where t = matrixToTable $ transpose m2

          row :: Int -> [Float]
          row n = [element n i | i <- [0 .. (length t) - 1]]

          element :: Int -> Int -> Float
          element i j = sum $ zipWith (\ a b -> a * b) (xs !! i) (t !! j)

transpose :: Matrix -> Matrix
transpose m@(Matrix xs) = Matrix [selectColumn i m | i <- [0 .. (length xs) - 1]]

selectColumn :: Int -> Matrix -> [Float]
selectColumn n (Matrix xs) = map (!! n) xs

matrixWithValues :: Int -> Int -> Float -> Matrix
matrixWithValues n m v = Matrix $ take n $ repeat $ take m $ repeat v

matrixToTable :: Matrix -> [[Float]]
matrixToTable (Matrix xs) = xs