module Vector (Vector,Scalar,Vector.sum, average, movingAverage, dotProduct, Vector.add, Vector.subtract,conv) where
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
     averaged = incMovingAverage startingWindow elements
     endingIndex = length elements
     returning = take endingIndex averaged
 in returning
dotProduct :: Vector -> Vector -> Scalar
dotProduct = elementwise (*) (+) 0

subtract :: Vector -> Vector -> Vector
subtract = elementwise (-) (:) []

add :: Vector -> Vector -> Vector
add = elementwise (+) (:) []

mul :: Vector -> Vector -> Vector
mul = elementwise (*) (:) []

elementwise toEach betweenEach base =
  let op [] [] = base
      op [] _  = error "incompatible uneven vectors"
      op v  [] = op [] v
      op (v1:vs1) (v2:vs2) = (toEach v1 v2) `betweenEach` (op vs1 vs2)
  in op

conv [] _ = []
conv _ [] = []
conv (a:ar) (b:br) = [a*b]
