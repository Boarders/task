module TextZipper where

-- text
import Data.Text

-- Core
import Bwd

data TextZipper = TextZipper
  { before_text :: Bwd Text
  , curr_text   :: Text
  , after_text  :: [Text]
  }
  
