module VectorConvolutionSpec (spec) where
import Vector
import TestHelper
import Test.Hspec
import Control.Exception(evaluate)
convTest :: (String,Vector,Scalar) -> Spec
convTest (name,elements, result) =
     it name $ do
       Vector.conv(elements,[0]) `shouldBe` result
convTestAll :: Spec
convTestAll =
    testAll "conv"
     convTest [("zero",[],0),
      ("one",[6],6),
      ("two",[6,7],13)]
spec :: Spec
spec = do
   convTestAll
