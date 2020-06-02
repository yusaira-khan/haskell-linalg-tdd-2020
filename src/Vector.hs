module Vector (Vector,Scalar,Vector.sum, average, movingAverage, dotProduct, Vector.add, Vector.subtract) where
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
dotProduct [] []  = 0
dotProduct [] _   = error "incompatible uneven vectors"
dotProduct v [] = dotProduct [] v
dotProduct (v1:vs1) (v2:vs2)  = v1*v2 + dotProduct vs1 vs2

subtract :: Vector -> Vector -> Vector
subtract [] []  = []
subtract [] _   = error "incompatible uneven vectors"
subtract v [] = Vector.subtract [] v
subtract (v1:vs1) (v2:vs2)  = (v1-v2): (Vector.subtract vs1 vs2)

add :: Vector -> Vector -> Vector
add [] []  = []
add [] _   = error "incompatible uneven vectors"
add v [] = Vector.add [] v
add (v1:vs1) (v2:vs2)  = (v1+v2): (Vector.add vs1 vs2)
