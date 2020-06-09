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
     convTest [("zero",[],0)

      ]
spec :: Spec
spec = do
   convTestAll
