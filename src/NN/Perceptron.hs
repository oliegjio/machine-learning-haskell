module NN.Perceptron where

import Algebra.Matrix

data Perceptron = Perceptron { layers :: [Matrix]
                             , biases :: [Matrix]
                             , weights :: [Matrix]
                             } deriving (Show, Eq)

perceptron :: [Int] -> Perceptron
perceptron c = Perceptron { layers = [zeroMatrix 1 i | i <- c]
                          , biases = [zeroMatrix 1 i | i <- tail c]
                          , weights = [zeroMatrix (c !! i) (c !! (i - 1)) | i <- [1 .. (length c) - 1]]
                          }