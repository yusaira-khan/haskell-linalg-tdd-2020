module Vector (Vector,Scalar,Vector.sum, average, movingAverage, dotProduct) where

type Vector = [Double]
type Scalar = Double
sum :: Vector -> Scalar
sum [] = 0
sum (x:xs) = x + (Vector.sum xs)
product = undefined
average' :: Vector -> Scalar
average' [] = 0
average' x= (Vector.sum x)/(fromIntegral $ length x)
divSum :: Scalar -> Vector -> Scalar
divSum _ [] = 0
divSum 0.0 _ = undefined
divSum l (x:xs) = (x/l) + (divSum l xs)
average'' :: Vector -> Scalar
average'' l = divSum (fromIntegral $ length l) l
average = average''
movingAverage :: Int -> Vector -> Vector
movingAverage _ [] =  []
movingAverage 0 _ =  []
movingAverage w (x:xs) =
  let
    currWindow = w
    currElements = [x]
    currAv = average currElements
    nextWindow = w
    nextElements = xs
  in currAv : (movingAverage nextWindow nextElements)
dotProduct = undefined
