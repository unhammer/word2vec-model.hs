import Test.Hspec

import Data.Word2Vec.Model

import qualified Data.Vector.Storable as V

import qualified Test.HUnit as HU

main :: IO ()
main = hspec $ do
  describe "basic facilities" $ do
    it "trivial dot product" $ do
      dotProduct (V.fromList [1.0, 2.0]) (V.fromList [1.0, 2.0]) `shouldBeAlmost` 5.0
    it "dot product" $ do
      dotProduct (V.fromList [0.5, 0.0, -2.0]) (V.fromList [3.0, 5.3, 1.0]) `shouldBeAlmost` (-0.5)
    it "cosine similarity (dissimilar)" $ do
      cosineSimilarity (getWVector [0.0, 4.3]) (getWVector [2.7, 0.0]) `shouldBeAlmost` 0.0
    it "cosine similarity (perfect similarity — trivial)" $ do
      cosineSimilarity (getWVector [1.0, 2.0]) (getWVector [1.0, 2.0]) `shouldBeAlmost` 1.0
    it "cosine similarity (perfect similarity)" $ do
      cosineSimilarity (getWVector [1.0, -3.7]) (getWVector [2.0, -7.4]) `shouldBeAlmost` 1.0

getWVector :: [Float] -> WVector
getWVector = buildWVector . V.fromList

class AEq a where
    (=~) :: a -> a -> Bool

instance AEq Float where
    x =~ y = abs ( x - y ) < (1.0e-4 :: Float)

(@=~?) :: (Show a, AEq a) => a -> a -> HU.Assertion
(@=~?) actual expected = expected =~ actual HU.@? assertionMsg
    where
      assertionMsg = "Expected : " ++ show expected ++
                     "\nActual   : " ++ show actual

shouldBeAlmost got expected = got @=~? expected

shouldReturnAlmost :: (AEq a, Show a, Eq a) => IO a -> a -> Expectation
shouldReturnAlmost action expected = action >>= (@=~? expected)
