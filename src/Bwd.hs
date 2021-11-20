module Bwd where

-- lens
import Control.Lens

data Bwd a where
  Nil   :: Bwd a
  (:|>) :: Bwd a -> a -> Bwd a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)
