import Lib 
import Test.Hspec (hspec, describe, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "Given a situation" $ do
    describe "When an event is occurred" $ do
      describe "Then it" $ do
        it "Should be True" $ do
          True `shouldBe` True
  describe "GET /expr?q=Hello,World" $ do
    it "Should response with Hello,World" $
        get "/expr?q=Hello,World" `
