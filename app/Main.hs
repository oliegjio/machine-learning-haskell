module Main where

import Algebra.Matrix

main :: IO ()
main = do
    let m1 = matrixWithValues 100000 100000 921
    let m2 = matrixWithValues 100000 100000 865
    let m3 = m1 |+| m2
    print (case m3 of Matrix xs -> xs !! 99999 !! 99999)
    print $ (selectColumn 1 m1) !! 1
    return ()