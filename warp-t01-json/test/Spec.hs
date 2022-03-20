import Lib ()
import Test.Hspec (hspec, describe, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "Given a situation" $ do
    describe "When an event is occurred" $ do
      describe "Then it" $ do
        it "Should be True" $ do
          True `shouldBe` True
