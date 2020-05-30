 module VectorSpec (spec) where

 import Vector
 import Test.Hspec

 sumTest :: (String,Vector,Scalar) -> Spec
 sumTest (name,elements, result) =
     it name $ do
       Vector.sum(elements) `shouldBe` result
 testList ::  (a ->  Spec ) -> [a] -> Spec
 testList f (t:[]) = f t
 testList f (t:ts) = f t >> testList f ts

 testAll :: String -> (a->Spec)  -> [a] -> Spec
 testAll name testfun testcases =
   do
     describe name $ do testList testfun testcases
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
      ("two window",2,[6,7],[6.0,6.5, 7]),
      ("extra window",5,[6,7],[6.0,6.5, 7])]
 spec :: Spec
 spec = do
   sumTestAll
   avgTestAll
   movAvgTestAll
