module Vector (Vector,Scalar,Vector.sum, average, movingAverage, dotProduct) where

type Vector = [Double]
type Scalar = Double
sum :: Vector -> Scalar
sum [] = 0
sum x:xs = x
product = undefined
average = undefined
movingAverage = undefined
dotProduct = undefined
