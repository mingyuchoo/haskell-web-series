import Lib
import Test.Hspec (hspec, describe, it, shouldBe)

-- |
--
--
main :: IO ()
main = hspec $ do
  describe "Given an initial context of the system" $ do
    describe "When an event or an action is occurred" $ do
      describe "Then it" $ do
        it "Should be true" $ do
          True `shouldBe` True
