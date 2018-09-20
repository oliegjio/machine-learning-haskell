module Algebra.Matrix where

data Matrix = Matrix [[Float]] deriving (Show, Eq)

matrixAdd :: Matrix -> Matrix -> Matrix
matrixAdd (Matrix xs) (Matrix ys) = Matrix $ zipWith (\ a b -> zipWith (+) a b) xs ys

matrixSubtract :: Matrix -> Matrix -> Matrix
matrixSubtract (Matrix xs) (Matrix ys) = Matrix $ zipWith (\ a b -> zipWith (-) a b) xs ys

hadamardProduct :: Matrix -> Matrix -> Matrix
hadamardProduct (Matrix xs) (Matrix ys) = Matrix $ zipWith (\ a b -> zipWith (*) a b) xs ys

matrixOnScalar :: Matrix -> Float -> Matrix
matrixOnScalar (Matrix xs) v = Matrix $ map (\ r -> map (\ e -> v * e) r) xs

matrixMultiply :: Matrix -> Matrix -> Matrix
matrixMultiply m1@(Matrix xs) m2@(Matrix ys) = Matrix [row n | n <- [0 .. (length xs) - 1]]
    where t = matrixToTable $ transpose m2

          row :: Int -> [Float]
          row n = [element n i | i <- [0 .. (length t) - 1]]

          element :: Int -> Int -> Float
          element i j = sum $ zipWith (\ a b -> a * b) (xs !! i) (t !! j)

transpose :: Matrix -> Matrix
transpose m@(Matrix xs) = Matrix [matrixColumn i m | i <- [0 .. (length xs) - 1]]

matrixColumn :: Int -> Matrix -> [Float]
matrixColumn n (Matrix xs) = map (!! n) xs

matrixRow :: Int -> Matrix -> [Float]
matrixRow n (Matrix xs) = xs !! n

matrixElement :: Int -> Int -> Matrix -> Float
matrixElement i j (Matrix xs) = xs !! i !! j

matrixWithValues :: Int -> Int -> Float -> Matrix
matrixWithValues n m v = Matrix $ take n $ repeat $ take m $ repeat v

matrixToTable :: Matrix -> [[Float]]
matrixToTable (Matrix xs) = xs

matrixMap :: (Float -> Float) -> Matrix -> Matrix
matrixMap f (Matrix xs) = Matrix $ map (\ r -> map (\ e -> f e) r) xs

zeroMatrix :: Int -> Int -> Matrix
zeroMatrix n m = matrixWithValues n m 0