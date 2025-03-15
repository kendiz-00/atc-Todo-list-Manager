-- src/Main.hs
module Main where

import System.IO (hFlush, stdout)
import System.Directory (doesFileExist)
import Database.SQLite.Simple
import CLI
import TaskManager
import Task
import Text.Read (readMaybe)

main :: IO ()
main = do
    putStrLn "Welcome to my TODO List Manager!"
    conn <- initializeDB
    mainMenu conn

-- Main menu
mainMenu :: Connection -> IO ()
mainMenu conn = do
    putStrLn "\nMain Menu:"
    putStrLn "1. View Tasks"
    putStrLn "2. Add a Task"
    putStrLn "3. Mark a Task as Complete"
    putStrLn "4. Delete a Task"
    putStrLn "5. Edit a Task"
    putStrLn "6. Search Tasks"
    putStrLn "7. Exit"
    putStr "Enter your choice: "
    hFlush stdout
    choice <- getLine
    case choice of
        "1" -> do
            putStrLn "\nViewing Tasks:"
            tasks <- viewTasks conn
            putStrLn $ show tasks
            mainMenu conn
        "2" -> do
            putStrLn "\nAdding a Task:"
            (isLooping, newTasks) <- addTaskPrompt conn
            if isLooping
                then mainMenu conn
                else putStrLn "Goodbye!"
        "3" -> do
            putStrLn "\nMarking a Task as Complete:"
            (isLooping, newTasks) <- completeTaskPrompt conn
            if isLooping
                then mainMenu conn
                else putStrLn "Goodbye!"
        "4" -> do
            putStrLn "\nDeleting a Task:"
            (isLooping, newTasks) <- deleteTaskPrompt conn
            if isLooping
                then mainMenu conn
                else putStrLn "Goodbye!"
        "5" -> do
            putStrLn "\nEditing a Task:"
            (isLooping, newTasks) <- editTaskPrompt conn
            if isLooping
                then mainMenu conn
                else putStrLn "Goodbye!"
        "6" -> do
            putStrLn "\nSearching Tasks:"
            (isLooping, newTasks) <- searchTaskPrompt conn
            if isLooping
                then mainMenu conn
                else putStrLn "Goodbye!"
        "7" -> putStrLn "Goodbye!"
        _ -> do
            putStrLn "Invalid choice. Please try again."
            mainMenu conn

-- Load tasks from file
loadTasks :: IO TaskList
loadTasks = do
  exists <- doesFileExist "tasks.txt"
  if exists
    then do
      content <- readFile "tasks.txt"
      case readMaybe content of
        Just tasks -> return tasks
        Nothing -> return []
    else return []

-- Save tasks to file
saveTasks :: TaskList -> IO ()
saveTasks tasks = writeFile "tasks.txt" (show tasks)