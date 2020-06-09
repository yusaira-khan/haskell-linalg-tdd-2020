module DataGenSpec(spec)
where
import TestHelper
import DataGen.Data
import Test.Hspec

kalTest ::(Show a,Eq a)  => (String,[a]) -> Spec
kalTest (name, hey) =
    it name $ do
       hey `shouldBe` []
kalTestWorkingAll :: Spec
kalTestWorkingAll =
   testAll "Kalman"
    kalTest [("zero", DataGen.Data.samples)
     ]

spec :: Spec
spec = do
  kalTestWorkingAll
