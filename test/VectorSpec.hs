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
      ] >> testAll "Dot Product error" dotTestError [([],[1]),([1],[]),([2,1],[1]),([2],[2,1])]
subTest :: (String,Vector,Vector,Vector) -> Spec
subTest (name,v1,v2, result) =
      it name $ do
        (Vector.subtract v1 v2) `shouldBe` result
subErrorName:: (Vector,Vector)-> String
subErrorName (v1,v2) ="sub bad "++ (show $length v1) ++ " and "++(show $length v2)
subTestError :: (Vector,Vector) -> Spec
subTestError (v1,v2) =
     it (subErrorName(v1,v2))$ (evaluate (Vector.subtract v1 v2) )`shouldThrow` errorCall "incompatible uneven vectors"
subTestWorkingAll :: Spec
subTestWorkingAll =
     testAll "Subtraction"
      subTest [("zero",[],[],[])
       ,("one",[6],[2],[4])
       ,("two",[6,1],[2,3],[4,-2])
       ] >> testAll "sub error" subTestError [([],[1]),([1],[])]
addTest :: (String,Vector,Vector,Vector) -> Spec
addTest (name,v1,v2, result) =
      it name $ do
        (Vector.add v1 v2) `shouldBe` result
addErrorName:: (Vector,Vector)-> String
addErrorName (v1,v2) ="add bad "++ (show $length v1) ++ " and "++(show $length v2)
addTestError :: (Vector,Vector) -> Spec
addTestError (v1,v2) =
     it (addErrorName(v1,v2))$ (evaluate (Vector.add v1 v2) )`shouldThrow` errorCall "incompatible uneven vectors"
addTestWorkingAll :: Spec
addTestWorkingAll =
     testAll "add"
      addTest [("zero",[],[],[])
       ,("one",[6],[2],[8])
       ,("two",[6,1],[2,3],[8,4])
       ] >> testAll "add error" addTestError [
   ([],[1]),([1],[])] -- these work but show bad error ([2,1],[1]),([2],[2,1])]
spec :: Spec
spec = do
   sumTestAll
   avgTestAll
   movAvgTestAll
   dotTestWorkingAll
   subTestWorkingAll
   addTestWorkingAll
