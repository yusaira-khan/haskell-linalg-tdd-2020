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
 spec :: Spec
 spec = do
   sumTestAll
   avgTestAll
