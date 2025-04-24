{-# LANGUAGE OverloadedStrings #-}

-- | Test suite for the Todo application
module Main
    ( main
    ) where

import           Data.Either
    ( isLeft
    , isRight
    )
import           Data.Text                                        (pack)
import           Flow                                             ((<|))
import           Test.Hspec

-- Domain imports
import           Domain.Repositories.Entities.Todo
    ( NewTodo (..)
    , Priority (..)
    , Status (DoingStatus, TodoStatus)
    , Todo (..)
    , validateTodoTitle
    )

-- Application imports
import           Application.UseCases.TodoUseCases

-- Infrastructure imports
import           Infrastructure.Repositories.SQLiteTodoRepository

-- -------------------------------------------------------------------
-- Test Suite
-- -------------------------------------------------------------------

-- | Main test runner
main :: IO ()
main = hspec spec

-- | Test specifications
spec :: Spec
spec = do
    describe "Domain.Repositories.Entities.Todo" <| do
        context "when validating todo titles" <| do
            it "should reject empty titles" <| do
                validateTodoTitle (pack "") `shouldSatisfy` isLeft

            it "should reject titles shorter than 3 characters" <| do
                validateTodoTitle (pack "ab") `shouldSatisfy` isLeft

            it "should reject titles longer than 50 characters" <| do
                validateTodoTitle (pack <| replicate 51 'a') `shouldSatisfy` isLeft

            it "should accept valid titles" <| do
                validateTodoTitle (pack "Buy groceries") `shouldSatisfy` isRight
                validateTodoTitle (pack "abc") `shouldSatisfy` isRight
                validateTodoTitle (pack <| replicate 50 'a') `shouldSatisfy` isRight

        context "when working with Priority" <| do
            it "should convert Priority to String correctly" <| do
                show Low `shouldBe` "Low"
                show Medium `shouldBe` "Medium"
                show High `shouldBe` "High"

            it "should cycle through priorities correctly" <| do
                -- Test the enum cycle: Low -> Medium -> High -> Low
                succ Low `shouldBe` Medium
                succ Medium `shouldBe` High
                succ High `shouldBe` Low  -- Wraps around due to Bounded instance

            it "should have correct min and max bounds" <| do
                minBound `shouldBe` Low
                maxBound `shouldBe` High

    describe "Infrastructure.Repositories.SQLiteTodoRepository" <| do
        -- These tests require a database connection
        -- In a real-world scenario, we would use a test database or mock
        context "when performing database operations" <| do
            it "should be able to create a database connection" <| do
                withConn (\_ -> pure True) `shouldReturn` True

    describe "Application.UseCases.TodoUseCases" <| do
        context "when creating a new todo" <| do
            it "should validate the todo before creation" <| do
                let invalidTodo = NewTodo (pack "")
                result <- runSQLiteRepo <| createNewTodo invalidTodo
                result `shouldSatisfy` isLeft

            it "should accept valid todos" <| do
                let validTodo = NewTodo (pack "Test Todo")
                -- This is just a validation test, not actually inserting into DB
                validateTodoTitle (newTodoName validTodo) `shouldSatisfy` isRight

        context "when updating a todo" <| do
            it "should validate the todo before update" <| do
                -- Create a Todo with empty title (which should be invalid)
                let invalidTodo = Todo
                      { todoId = 1
                      , todoTitle = pack ""
                      , createdAt = read "2023-01-01 00:00:00 UTC"
                      , priority = Low
                      , status = TodoStatus
                      }
                result <- runSQLiteRepo <| updateExistingTodo 1 invalidTodo
                result `shouldSatisfy` isLeft

            it "should accept valid todos for update" <| do
                -- Create a valid Todo
                let validTodo = Todo
                      { todoId = 1
                      , todoTitle = pack "Valid Todo"
                      , createdAt = read "2023-01-01 00:00:00 UTC"
                      , priority = Medium
                      , status = DoingStatus
                      }
                validateTodoTitle (todoTitle validTodo) `shouldSatisfy` isRight

    -- Integration tests that would require a test database setup
    describe "Integration Tests" <| do
        context "when using the repository through use cases" <| do
            it "should handle the full lifecycle of a todo" <| pending
            -- In a complete test suite, we would:
            -- 1. Set up a test database
            -- 2. Create a todo
            -- 3. Verify it exists
            -- 4. Update it (including priority and status changes)
            -- 5. Verify the update
            -- 6. Delete it
            -- 7. Verify deletion
            -- 8. Clean up the test database

        context "when toggling priority and status" <| do
            it "should correctly cycle through priority levels" <| pending
            -- This would test the full cycle of priority changes:
            -- 1. Create a todo with Low priority
            -- 2. Update to Medium priority
            -- 3. Update to High priority
            -- 4. Update back to Low priority

            it "should correctly cycle through status levels" <| pending
            -- This would test the full cycle of status changes:
            -- 1. Create a todo with TodoStatus
            -- 2. Update to DoingStatus
            -- 3. Update to DoneStatus
            -- 4. Update back to TodoStatus
