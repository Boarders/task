module Task where

-- text
import qualified Data.Text as Text
import Data.Text (Text)

-- lens
import Control.Lens

-- Core
import TaskUID

data Status = Done | Unfinished
  deriving stock (Eq, Ord, Show)

data TaskF t = Task
  { text    :: !t
  , status  :: !Status
  , task_uid :: !TaskUID
  }
  deriving stock (Eq, Ord, Show, Functor)

type Task = TaskF Text

_text :: Lens (TaskF t) (TaskF t') t t'
_text = lens text (\tsk txt -> tsk {text = txt})

_status :: Lens' (TaskF t) Status
_status = lens status (\tsk stat -> tsk {status = stat})

_task_uid :: Lens' (TaskF t) TaskUID
_task_uid = lens task_uid (\tsk tuid -> tsk {task_uid = tuid})

firstTask :: t -> Status -> TaskF t
firstTask t s = Task t s newTaskUID

newTask :: t -> Status -> Int -> TaskF t
newTask t s n = Task t s (incTaskUID n $ newTaskUID)
