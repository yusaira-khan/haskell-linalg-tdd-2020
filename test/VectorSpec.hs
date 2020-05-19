 module VectorSpec (spec) where

 import Vector
 import Test.Hspec

 sumTest :: (String,Vector,Scalar) -> Spec
 sumTest (name,elements, result) =
     it name $ do
       Vector.sum(elements) `shouldBe` result
 sumTestAll :: Spec
 sumTestAll =
   do
   describe "Sum" $ do
     sumTest ("zero",[],0)
 spec :: Spec
 spec = do
   sumTestAll
