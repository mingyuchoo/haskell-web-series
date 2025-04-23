{-# LANGUAGE OverloadedStrings #-}

module Presentation.Web.Templates
    ( indexTemplate
    , baseTemplate
    ) where

import           Data.Text                      (Text, pack)
import           Domain.Entities.Todo           (Todo(..))
import           Lucid

-- Base template with common elements
baseTemplate :: Text -> Html () -> Html () -> Html ()
baseTemplate title headContent bodyContent = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
    title_ (toHtml title)
    link_ [rel_ "stylesheet", href_ "/static/css/style.css"]
    headContent
  body_ $ do
    div_ [class_ "container"] $ do
      h1_ (toHtml title)
      bodyContent
    script_ [src_ "/static/js/main.js"] ("" :: Text)

-- Index page template
indexTemplate :: [Todo] -> Html ()
indexTemplate todos = baseTemplate "Todo Management" mempty $ do
  div_ [id_ "message-container"] mempty

  -- Todo form
  div_ [class_ "container"] $ do
    h2_ [id_ "form-title"] "Create New Todo"
    form_ [id_ "todo-form"] $ do
      input_ [type_ "hidden", id_ "form-mode", name_ "form-mode", value_ "create"]
      input_ [type_ "hidden", id_ "todoId", name_ "todoId"]
      div_ [class_ "form-group"] $ do
        label_ [for_ "todoTitle"] "Todo Title:"
        input_ [type_ "text", id_ "todoTitle", name_ "todoTitle", required_ "required"]
      button_ [type_ "submit", class_ "btn btn-success", id_ "submit-btn"] "Create Todo"
      button_ [type_ "button", class_ "btn", onclick_ "resetForm()"] "Reset"

  -- Todos table
  div_ [class_ "container"] $ do
    h2_ "Todo List"
    table_ $ do
      thead_ $ do
        tr_ $ do
          th_ "ID"
          th_ "Todo"
          th_ "Actions"
      tbody_ [id_ "todos-table-body"] $ do
        if null todos
          then tr_ $ td_ [colspan_ "3"] "No todos found"
          else mapM_ todoRow todos

-- Single todo row template
todoRow :: Todo -> Html ()
todoRow todo = tr_ $ do
  td_ (toHtml $ show $ todoId todo)
  td_ (toHtml $ todoTitle todo)
  td_ $ do
    button_ [class_ "btn", onclick_ $ "editTodo(" <> pack (show $ todoId todo) <> ", '" <> pack (todoTitle todo) <> "')"]
      "Edit"
    button_ [class_ "btn btn-danger", onclick_ $ "deleteTodo(" <> pack (show $ todoId todo) <> ")"]
      "Delete"
