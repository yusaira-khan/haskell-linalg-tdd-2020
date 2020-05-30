module TestHelper(testAll) where
import Test.Hspec

testList ::  (a ->  Spec ) -> [a] -> Spec
testList f (t:[]) = f t
testList f (t:ts) = f t >> testList f ts

testAll :: String -> (a->Spec)  -> [a] -> Spec
testAll name testfun testcases =
  do
    describe name $ do testList testfun testcases
