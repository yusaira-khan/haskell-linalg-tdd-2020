module VectorConvolutionSpec (spec) where
import Vector
import TestHelper
import Test.Hspec
import Control.Exception(evaluate)
convTest :: (String,Vector,Vector,Vector) -> Spec
convTest (name,v1, v2, r) =
     it name $ do
       (Vector.conv v1 v2) `shouldBe` r
convTestAll :: Spec
convTestAll =
    testAll "conv"
     convTest [("zero_zero",[],[],[])
              ,("zero_notzero",[1],[],[])
              ,("notzero_zero",[],[1],[])

      ]
spec :: Spec
spec = do
   convTestAll
