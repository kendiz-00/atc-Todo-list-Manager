-- src/TaskManager.hs
module TaskManager where

import Task
import Data.Time (Day)
import Data.List (intercalate, sortBy, isInfixOf)
import Data.Ord (comparing)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import qualified Data.Text as T

-- Initialize the database
initializeDB :: IO Connection
initializeDB = do
    conn <- open "todo.db"
    execute_ conn (Query $ T.pack "CREATE TABLE IF NOT EXISTS tasks ( \
                 \id INTEGER PRIMARY KEY AUTOINCREMENT, \
                 \description TEXT NOT NULL, \
                 \is_complete BOOLEAN NOT NULL, \
                 \priority TEXT NOT NULL, \
                 \due_date TEXT, \
                 \category TEXT NOT NULL)")
    return conn

instance FromRow Task where
    fromRow = Task <$> field
                   <*> field
                   <*> field
                   <*> field
                   <*> (fmap read <$> field)  -- Convert Maybe String to Maybe Day
                   <*> field

-- Add a new task
addTask :: Connection -> T.Text -> Priority -> Maybe Day -> Category -> IO Task
addTask conn desc priority dueDate category = do
    let dueDateStr = T.pack . show <$> dueDate
    execute conn (Query $ T.pack "INSERT INTO tasks (description, is_complete, priority, due_date, category) \
                 \VALUES (?, ?, ?, ?, ?)") (desc, False, T.pack (show priority), dueDateStr, T.pack (show category))
    lastId <- lastInsertRowId conn
    return $ Task (fromIntegral lastId) desc False priority dueDate category

-- View all tasks
viewTasks :: Connection -> IO TaskList
viewTasks conn = query_ conn (Query $ T.pack "SELECT * FROM tasks")

-- Mark a task as complete
markTaskComplete :: Connection -> Int -> IO ()
markTaskComplete conn taskId = do
    execute conn (Query $ T.pack "UPDATE tasks SET is_complete = ? WHERE id = ?") (True, taskId)

-- Delete a task
deleteTask :: Connection -> Int -> IO ()
deleteTask conn taskId = do
    execute conn (Query $ T.pack "DELETE FROM tasks WHERE id = ?") (Only taskId)

-- Edit a task's description
editTask :: TaskList -> Int -> T.Text -> TaskList
editTask tasks targetId newDesc = map (\task -> if taskId task == targetId then task { description = newDesc } else task) tasks

-- Sort tasks by due date
sortByDueDate :: TaskList -> TaskList
sortByDueDate = sortBy (comparing dueDate)

-- Sort tasks by priority
sortByPriority :: TaskList -> TaskList
sortByPriority = sortBy (comparing priority)

-- Sort tasks by category
sortByCategory :: TaskList -> TaskList
sortByCategory = sortBy (comparing category)

-- Filter tasks by completion status
filterByCompletion :: Bool -> TaskList -> TaskList
filterByCompletion status = filter (\task -> isComplete task == status)

-- Filter tasks by priority
filterByPriority :: Priority -> TaskList -> TaskList
filterByPriority p = filter (\task -> priority task == p)

-- Filter tasks by category
filterByCategory :: Category -> TaskList -> TaskList
filterByCategory c = filter (\task -> category task == c)

-- Search tasks by description
searchTasks :: String -> TaskList -> TaskList
searchTasks query = filter (\task -> query `isInfixOf` T.unpack (description task))