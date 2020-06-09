module DataGenSpec(spec)
where
import TestHelper
import DataGen.Data
import Vector
import Test.Hspec

kalTest :: (String,Vector) -> Spec
kalTest (name, hey) =
    it name $ do
       hey `shouldBe`  (movingAverage 2 hey)
kalTestWorkingAll :: Spec
kalTestWorkingAll =
   testAll "Kalman"
    kalTest [("zero", DataGen.Data.samples)
     ]

spec :: Spec
spec = do
  kalTestWorkingAll
