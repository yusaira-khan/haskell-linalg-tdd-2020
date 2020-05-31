module VectorSpec (spec) where

import Vector
import TestHelper
import Test.Hspec
import Control.Exception(evaluate)

sumTest :: (String,Vector,Scalar) -> Spec
sumTest (name,elements, result) =
     it name $ do
       Vector.sum(elements) `shouldBe` result
sumTestAll :: Spec
sumTestAll =
    testAll "Sum"
     sumTest [("zero",[],0),
      ("one",[6],6),
      ("two",[6,7],13)]

avgTest :: (String,Vector,Scalar) -> Spec
avgTest (name,elements, result) =
     it name $ do
       Vector.average(elements) `shouldBe` result
avgTestAll :: Spec
avgTestAll =
    testAll "Average"
     avgTest [("zero",[],0),
      ("one",[6],6),
      ("two",[6,7],6.5)]
movAvgTest :: (String,Int,Vector,Vector) -> Spec
movAvgTest (name,window,elements, result) =
     it name $ do
       (Vector.movingAverage window elements) `shouldBe` result
movAvgTestAll :: Spec
movAvgTestAll =
    testAll "MovingAverage"
     movAvgTest [("zero array", 5,[],[]),
      ("zero window",0,[6],[]),
      ("one same",1,[6],[6]),
      ("one window",1,[6,7],[6,7]),
      ("two window",2,[6,7],[6.0,6.5]),
      ("extra window",5,[6,7],[6.0,6.5])]
dotTest :: (String,Vector,Vector,Scalar) -> Spec
dotTest (name,v1,v2, result) =
     it name $ do
       (Vector.dotProduct v1 v2) `shouldBe` result
dotErrorName:: (Vector,Vector)-> String
dotErrorName (v1,v2) ="dot product bad "++ (show $length v1) ++ " and "++(show $length v2)
dotTestError :: (Vector,Vector) -> Spec
dotTestError (v1,v2) =
    it (dotErrorName(v1,v2))$ (evaluate (Vector.dotProduct v1 v2) )`shouldThrow` errorCall "incompatible uneven vectors"
dotTestWorkingAll :: Spec
dotTestWorkingAll =
    testAll "Dot Product"
     dotTest [("zero",[],[],0)
      ,("one",[6],[2],12)
      ,("two",[6,7],[2,3],33)
      ] >> testAll "Dot Product error" dotTestError [
  ([],[1]),([1],[]),([2,1],[1]),([2],[2,1])
                                                    ]
spec :: Spec
spec = do
   sumTestAll
   avgTestAll
   movAvgTestAll
   dotTestWorkingAll
