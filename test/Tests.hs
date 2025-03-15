-- test/Tests.hs
module Main where

import Test.HUnit
import TaskManager
import Task
import CLI
import qualified Data.Text as T
import Database.SQLite.Simple (open, close, Connection)
import System.Directory (removeFile)

-- Create a temporary database for testing
withTestDB :: (Connection -> IO a) -> IO a
withTestDB action = do
  let dbFile = "test.db"
  conn <- open dbFile
  result <- action conn
  close conn
  removeFile dbFile
  return result

-- Test data
testTask1 :: Task
testTask1 = Task 1 (T.pack "Buy groceries") False High Nothing Personal

testTask2 :: Task
testTask2 = Task 2 (T.pack "Clean the house") False Medium Nothing Work

testTaskList :: TaskList
testTaskList = [testTask1, testTask2]

-- Test cases
testAddTask :: Test
testAddTask = TestCase $ withTestDB $ \conn -> do
  _ <- addTask conn (T.pack "Submit report") Low Nothing Work
  tasks <- viewTasks conn
  assertEqual "Adding a task should increase the list size" 1 (length tasks)
  assertEqual "The new task should have the correct description" (T.pack "Submit report") (description (head tasks))

testMarkTaskComplete :: Test
testMarkTaskComplete = TestCase $ withTestDB $ \conn -> do
  _ <- addTask conn (T.pack "Test task") High Nothing Work
  markTaskComplete conn 1
  tasks <- viewTasks conn
  assertEqual "Marking a task as complete should update its status" True (isComplete (head tasks))

testDeleteTask :: Test
testDeleteTask = TestCase $ withTestDB $ \conn -> do
  _ <- addTask conn (T.pack "Test task") High Nothing Work
  deleteTask conn 1
  tasks <- viewTasks conn
  assertEqual "Deleting a task should decrease the list size" 0 (length tasks)

testEditTask :: Test
testEditTask = TestCase $ do
  let newTaskList = editTask testTaskList 1 (T.pack "Buy milk and eggs")
  assertEqual "Editing a task should update its description" "Buy milk and eggs" (T.unpack (description (head newTaskList)))

-- Test suite
tests :: Test
tests = TestList
  [ TestLabel "testAddTask" testAddTask
  , TestLabel "testMarkTaskComplete" testMarkTaskComplete
  , TestLabel "testDeleteTask" testDeleteTask
  , TestLabel "testEditTask" testEditTask
  ]

-- Main function to run tests
main :: IO ()
main = do
  runTestTT tests
  return ()