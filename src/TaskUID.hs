module TaskUID where

-- chronos
import qualified Chronos as Chronos

-- text
import Data.Text

-- lens
import Control.Lens

data TaskUID = TaskUID
  { taskIndex :: !Int
  }
  deriving (Eq, Ord, Show)

_taskIndex :: Lens' TaskUID Int
_taskIndex = lens taskIndex (\tuid ct -> tuid {taskIndex = ct})

newTaskUID :: TaskUID
newTaskUID = TaskUID {taskIndex = 0}

incTaskUID :: Int -> TaskUID -> TaskUID
incTaskUID n = over _taskIndex (+ n)

