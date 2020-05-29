module Vector (Vector,Scalar,Vector.sum, average, movingAverage, dotProduct,kalmanfilter) where

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
         maxWindow =
           if (window > length currElements)
           then length currElements
           else window
         avWindow =
           if currWindow < maxWindow
           then currWindow
           else maxWindow
         avElements = take avWindow currElements
         av = average avElements
         nextWindow =
           if currWindow < maxWindow
           then currWindow+1
           else maxWindow
         nextElements =
           if currWindow < maxWindow
           then currElements
           else tail currElements
       in av : (incMovingAverage nextWindow nextElements)
     startingWindow = if window < 1 then 0 else 1
  in incMovingAverage startingWindow elements
dotProduct = undefined

kalmanfilter  = undefined
