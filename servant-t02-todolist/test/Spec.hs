import Test.Hspec
import Data.Either (isLeft, isRight)

-- Domain imports
import Domain.Repositories.Entities.Todo

-- Application imports
import Application.UseCases.TodoUseCases

-- Infrastructure imports
import Infrastructure.Repositories.SQLiteTodoRepository

-- -------------------------------------------------------------------
-- Test
-- -------------------------------------------------------------------

-- Main function
main :: IO ()
main = hspec spec

-- Test suite
spec :: Spec
spec = do
    describe "Domain.Repositories.Entities.Todo" $ do
        context "when validating todo titles" $ do
            it "should reject empty titles" $ do
                validateTodoTitle "" `shouldSatisfy` isLeft
                
            it "should reject titles shorter than 3 characters" $ do
                validateTodoTitle "ab" `shouldSatisfy` isLeft
                
            it "should reject titles longer than 50 characters" $ do
                validateTodoTitle (replicate 51 'a') `shouldSatisfy` isLeft
                
            it "should accept valid titles" $ do
                validateTodoTitle "Buy groceries" `shouldSatisfy` isRight
                validateTodoTitle "abc" `shouldSatisfy` isRight
                validateTodoTitle (replicate 50 'a') `shouldSatisfy` isRight
                
        context "when working with Priority" $ do
            it "should convert Priority to String correctly" $ do
                show Low `shouldBe` "Low"
                show Medium `shouldBe` "Medium"
                show High `shouldBe` "High"
    
    describe "Infrastructure.Repositories.SQLiteTodoRepository" $ do
        -- These tests require a database connection
        -- In a real-world scenario, we would use a test database or mock
        context "when performing database operations" $ do
            it "should be able to create a database connection" $ do
                withConn (\_ -> return True) `shouldReturn` True
                
    describe "Application.UseCases.TodoUseCases" $ do
        context "when creating a new todo" $ do
            it "should validate the todo before creation" $ do
                let invalidTodo = NewTodo ""
                result <- runSQLiteRepo $ createNewTodo invalidTodo
                result `shouldSatisfy` isLeft
                
            it "should accept valid todos" $ do
                let validTodo = NewTodo "Test Todo"
                -- This is just a validation test, not actually inserting into DB
                validateTodoTitle (newTodoName validTodo) `shouldSatisfy` isRight
                
    -- Integration tests that would require a test database setup
    describe "Integration Tests" $ do
        context "when using the repository through use cases" $ do
            it "should handle the full lifecycle of a todo" $ pending
            -- In a complete test suite, we would:
            -- 1. Set up a test database
            -- 2. Create a todo
            -- 3. Verify it exists
            -- 4. Update it
            -- 5. Verify the update
            -- 6. Delete it
            -- 7. Verify deletion
            -- 8. Clean up the test database
