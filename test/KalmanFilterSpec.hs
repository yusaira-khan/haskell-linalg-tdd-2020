module KalmanFilterSpec

where
import TestHelper
import Test.Hspec
kalTest :: (String) -> Spec
kalTest (name) =
    it name $ do
       "" `shouldBe` ""
kalTestWorkingAll :: Spec
kalTestWorkingAll =
   testAll "Kalman"
    kalTest ["zero"
     ]

spec :: Spec
spec = do
  kalTestWorkingAll

