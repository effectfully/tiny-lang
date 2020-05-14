-- | Basic structure of our programs

module TinyLang.Field.Core
    ( Program (..)
    , Statements (..)
    ) where

import           GHC.Generics
import           Quiet

-- | Basic wrapper of statements
newtype Statements stmt = Statements { unStatements :: [stmt] }
    deriving (Generic, Eq, Functor, Foldable, Traversable)
    deriving (Show) via (Quiet (Statements stmt))

-- | Basic wrapper of program
newtype Program stmt = Program { unProgram :: (Statements stmt) }
    deriving (Generic, Eq, Functor, Foldable, Traversable)
    deriving (Show) via (Quiet (Program stmt))
