{-# LANGUAGE OverloadedStrings #-}

module Presentation.Web.Templates
    ( indexTemplate
    , baseTemplate
    ) where

import           Data.Text                      (Text, pack)
import           Data.Time                      (UTCTime, formatTime, defaultTimeLocale)
import           Lucid
import           Domain.Repositories.Entities.Todo (Todo(..), Priority(..), Status(..))


-- -------------------------------------------------------------------
-- Templates
-- -------------------------------------------------------------------

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
      
      div_ [class_ "form-row"] $ do
        div_ [class_ "form-group priority-group"] $ do
          label_ [for_ "todoPriority"] "Priority:"
          select_ [id_ "todoPriority", name_ "todoPriority"] $ do
            option_ [value_ "Low"] "Low"
            option_ [value_ "Medium"] "Medium"
            option_ [value_ "High"] "High"
        
        div_ [class_ "form-group", id_ "status-group"] $ do
          label_ [for_ "todoStatus"] "Status:"
          select_ [id_ "todoStatus", name_ "todoStatus"] $ do
            option_ [value_ "Todo"] "Todo"
            option_ [value_ "Doing"] "Doing"
            option_ [value_ "Done"] "Done"
      
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
          th_ "Created At"
          th_ "Priority"
          th_ "Status"
          th_ "Actions"
      tbody_ [id_ "todos-table-body"] $ do
        if null todos
          then tr_ $ td_ [colspan_ "6"] "No todos found"
          else mapM_ todoRow todos

-- Format UTC time for display
formatTodoTime :: UTCTime -> Text
formatTodoTime time = pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" time

-- Format UTC time as ISO 8601 for data attribute
formatISOTime :: UTCTime -> Text
formatISOTime time = pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" time

-- Format priority for display with color
formatPriority :: Priority -> Html ()
formatPriority Low = span_ [class_ "priority-low"] "Low"
formatPriority Medium = span_ [class_ "priority-medium"] "Medium"
formatPriority High = span_ [class_ "priority-high"] "High"

-- Convert Priority to string for JavaScript
priorityToString :: Priority -> Text
priorityToString Low = "Low"
priorityToString Medium = "Medium"
priorityToString High = "High"

-- Format status
formatStatus :: Status -> Html ()
formatStatus DoneStatus = span_ [class_ "status-completed", data_ "status" "done", onclick_ "toggleStatus(this)"] "Done"
formatStatus DoingStatus = span_ [class_ "status-doing", data_ "status" "doing", onclick_ "toggleStatus(this)"] "Doing"
formatStatus TodoStatus = span_ [class_ "status-pending", data_ "status" "todo", onclick_ "toggleStatus(this)"] "Todo"

-- Single todo row template
todoRow :: Todo -> Html ()
todoRow todo = tr_ [data_ "todo-id" (pack $ show $ todoId todo)] $ do
  td_ (toHtml $ show $ todoId todo)
  td_ (toHtml $ todoTitle todo)
  td_ [class_ "relative-time", data_ "timestamp" (formatISOTime $ createdAt todo)] (toHtml $ formatTodoTime $ createdAt todo)
  td_ (formatPriority $ priority todo)
  td_ (formatStatus $ status todo)
  td_ $ do
    button_ [class_ "btn", onclick_ $ "editTodo(" <> pack (show $ todoId todo) <> ", '" <> pack (todoTitle todo) <> "', '" <> priorityToString (priority todo) <> "', '" <> pack (show $ status todo) <> "')"]
      "Edit"
    button_ [class_ "btn btn-danger", onclick_ $ "deleteTodo(" <> pack (show $ todoId todo) <> ")"]
      "Delete"
