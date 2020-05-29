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
movingAverage window elements =
 let incMovingAverage _ [] =  []
     incMovingAverage 0 _ =  []
     incMovingAverage currWindow currElements =
       let
         maxWindow = window
          --  if (window > length currElements)
          --  then length currElements
          --  else window
         -- currWindow =if currWindow < mah
         avElements = [head currElements]
         av = average avElements
         nextWindow = currWindow
         nextElements = tail currElements
       in av : (incMovingAverage nextWindow nextElements)
     startingWindow = window
  in incMovingAverage startingWindow elements
dotProduct = undefined
