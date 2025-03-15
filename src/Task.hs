-- src/Task.hs
module Task where

import Data.Time (Day)
import qualified Data.Text as T
import Database.SQLite.Simple.FromField (FromField(..))
import Database.SQLite.Simple.ToField (ToField(..))

data Priority = High | Medium | Low deriving (Show, Read, Eq, Ord)
data Category = Work | Personal | Other deriving (Show, Read, Eq, Ord)

data Task = Task
  { taskId :: Int
  , description :: T.Text
  , isComplete :: Bool
  , priority :: Priority
  , dueDate :: Maybe Day
  , category :: Category
  } deriving (Show, Read)

-- Define TaskList as a type alias for [Task]
type TaskList = [Task]

-- FromField instance for Priority
instance FromField Priority where
    fromField f = do
        textVal <- fromField f
        return $ read (T.unpack textVal)

-- FromField instance for Category
instance FromField Category where
    fromField f = do
        textVal <- fromField f
        return $ read (T.unpack textVal)

-- ToField instance for Priority
instance ToField Priority where
    toField = toField . T.pack . show

-- ToField instance for Category
instance ToField Category where
    toField = toField . T.pack . show