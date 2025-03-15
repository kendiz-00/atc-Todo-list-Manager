-- src/CLI.hs
module CLI where

import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import Data.Time (Day)
import Database.SQLite.Simple
import TaskManager
import Task
import qualified Data.Text as T

-- Handle user input
handleInput :: Connection -> TaskList -> String -> IO (Bool, TaskList)
handleInput conn tasks "help" = do
    putStrLn "Available commands:"
    putStrLn "  add <description> <priority> <category> - Add a new task"
    putStrLn "  view                                   - View all tasks"
    putStrLn "  complete <id>                          - Mark a task as complete"
    putStrLn "  delete <id>                            - Delete a task"
    putStrLn "  edit <id> <new desc>                   - Edit a task"
    putStrLn "  sort due                               - Sort tasks by due date"
    putStrLn "  sort priority                          - Sort tasks by priority"
    putStrLn "  sort category                          - Sort tasks by category"
    putStrLn "  filter complete                        - Show completed tasks"
    putStrLn "  filter incomplete                      - Show incomplete tasks"
    putStrLn "  filter priority <priority>             - Filter tasks by priority"
    putStrLn "  filter category <category>             - Filter tasks by category"
    putStrLn "  search <query>                         - Search tasks by description"
    putStrLn "  exit                                   - Exit the application"
    putStrLn "  help                                   - Display this help menu"
    pure (True, tasks)

handleInput conn tasks "view" = do
    tasks' <- viewTasks conn
    putStrLn $ show tasks'
    pure (True, tasks')

handleInput conn tasks "complete" = completeTaskPrompt conn
handleInput conn tasks "delete" = deleteTaskPrompt conn
handleInput conn tasks "edit" = editTaskPrompt conn
handleInput conn tasks "search" = searchTaskPrompt conn

-- Add task prompt
addTaskPrompt :: Connection -> IO (Bool, TaskList)
addTaskPrompt conn = do
    putStr "Enter task description: "
    hFlush stdout
    desc <- T.pack <$> getLine
    putStr "Enter task priority (High, Medium, Low): "
    hFlush stdout
    priorityStr <- getLine
    putStr "Enter task category (Work, Personal, Other): "
    hFlush stdout
    categoryStr <- getLine
    putStr "Enter due date (YYYY-MM-DD, or leave blank): "
    hFlush stdout
    dueDateStr <- getLine
    let priority = read priorityStr :: Priority
    let category = read categoryStr :: Category
    let dueDate = if null dueDateStr
                  then Nothing
                  else case readMaybe dueDateStr of
                         Just day -> Just day
                         Nothing -> Nothing
    newTask <- addTask conn desc priority dueDate category
    putStrLn $ "Added task: " ++ T.unpack desc ++ " (Priority: " ++ show priority ++ ", Category: " ++ show category ++ ")"
    tasks <- viewTasks conn
    pure (True, tasks)

-- Complete task prompt
completeTaskPrompt :: Connection -> IO (Bool, TaskList)
completeTaskPrompt conn = do
    tasks <- viewTasks conn
    putStrLn $ show tasks
    putStr "Enter the ID of the task to mark as complete: "
    hFlush stdout
    taskIdStr <- getLine
    let taskId = read taskIdStr :: Int
    markTaskComplete conn taskId
    tasks' <- viewTasks conn
    putStrLn $ "Marked task " ++ show taskId ++ " as complete."
    pure (True, tasks')

-- Delete task prompt
deleteTaskPrompt :: Connection -> IO (Bool, TaskList)
deleteTaskPrompt conn = do
    tasks <- viewTasks conn
    putStrLn $ show tasks
    putStr "Enter the ID of the task to delete: "
    hFlush stdout
    taskIdStr <- getLine
    let taskId = read taskIdStr :: Int
    deleteTask conn taskId
    tasks' <- viewTasks conn
    putStrLn $ "Deleted task " ++ show taskId ++ "."
    pure (True, tasks')

-- Edit task prompt
editTaskPrompt :: Connection -> IO (Bool, TaskList)
editTaskPrompt conn = do
    tasks <- viewTasks conn
    putStrLn $ show tasks
    putStr "Enter the ID of the task to edit: "
    hFlush stdout
    taskIdStr <- getLine
    let taskId = read taskIdStr :: Int
    putStr "Enter the new description: "
    hFlush stdout
    newDesc <- T.pack <$> getLine
    let newTasks = editTask tasks taskId newDesc
    tasks' <- viewTasks conn
    putStrLn $ "Edited task " ++ show taskId ++ "."
    pure (True, tasks')

-- Search task prompt
searchTaskPrompt :: Connection -> IO (Bool, TaskList)
searchTaskPrompt conn = do
    putStr "Enter search query: "
    hFlush stdout
    query <- getLine
    tasks <- viewTasks conn
    let results = searchTasks query tasks
    if null results
        then putStrLn "No tasks found matching your query."
        else putStrLn $ "Search results for \"" ++ query ++ "\":\n" ++ show results
    pure (True, tasks)