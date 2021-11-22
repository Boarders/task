module TaskZipper where

-- base
import Control.Category ((>>>))

-- lens
import Control.Lens

-- text
import Data.Text as Text

-- Core
import Bwd
import Task
import TaskUID
import LineCursor

type CurrTask = TaskF LineCursor

data TaskZipper = TaskZipper
  { before_curr :: Bwd Task
  , curr_task   :: CurrTask
  , after_curr  :: [Task]
  }
  deriving (Show)

emptyTaskZipper :: TaskZipper
emptyTaskZipper = TaskZipper Nil (newTask lcEmpty Done 0) []

_before_curr :: Lens' TaskZipper (Bwd Task)
_before_curr = lens before_curr (\tz bc -> tz {before_curr = bc})

_after_curr :: Lens' TaskZipper [Task]
_after_curr = lens after_curr (\tz ac -> tz {after_curr = ac})

_curr_task :: Lens' TaskZipper CurrTask
_curr_task = lens curr_task (\tz ct -> tz {curr_task = ct})

currTaskText :: CurrTask -> Text
currTaskText = view _text >>> lcToText

currTaskToTask :: CurrTask -> Task
currTaskToTask = over _text lcToText

taskToCurrTask :: Task -> CurrTask
taskToCurrTask = over _text lcWithText

prev_task :: TaskZipper -> TaskZipper
prev_task tz@(TaskZipper Nil t ts) = tz
prev_task tz@(TaskZipper (bts :|> b) c ts) =
    TaskZipper bts (taskToCurrTask b) ((currTaskToTask c) : ts)

next_task :: TaskZipper -> TaskZipper
next_task tz@(TaskZipper btw t []) = tz
next_task tz@(TaskZipper bts c (t:ts)) =
    TaskZipper (bts :|> (currTaskToTask c)) (taskToCurrTask t) ts
