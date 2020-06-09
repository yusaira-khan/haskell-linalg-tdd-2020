module VectorConvolutionSpec (spec) where
import Vector
import TestHelper
import Test.Hspec
import Control.Exception(evaluate)
convTest :: (String,Vector,Vector,Vector) -> Spec
convTest (name,v1, v2, r) =
     it name $ do
       (Vector.conv v1 v2) `shouldBe` r
spec :: Spec
spec = do
    testAll "conv"
     convTest [("zero zero",[],[],[])
              ,("zero notzero",[1],[],[])
              ,("notzero zero",[],[1],[])
              ,("one one",[5],[3],[15])
              --,("one two",[5],[1,2],[5,10])
              --,("two two",[3,5],[1,2],[3,11,10])
      ]
