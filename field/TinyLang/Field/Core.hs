-- | Basic structure of our programs


module TinyLang.Field.Core
    ( Program (..)
    , Statements (..)
    , progSubExt
    , progSubStatements
    , progSubStatement
    , stmtsSubStatement
    ) where

import           Data.Bifunctor
import           Control.Lens
import           GHC.Generics
import           Quiet

-- | Basic wrapper of statements
newtype Statements stmt = Statements { unStatements :: [stmt] }
    deriving (Generic, Eq, Functor, Foldable, Traversable)
    deriving (Show) via (Quiet (Statements stmt))

-- | Basic wrapper of program
data Program ext stmt = Program
    { _programExts :: [ext]
    , _programStatements :: Statements stmt
    }
    deriving (Eq, Foldable, Traversable, Functor)

instance Bifunctor Program where
    bimap f g (Program exts stmts) = Program (fmap f exts) (fmap g stmts)

-- NOTE:  Adding explicit Show instance to avoid record syntax
instance (Show ext, Show stmt) => Show (Program ext stmt) where
    show (Program exts stmts) = "Program " ++ show exts ++ " " ++ show stmts

-- Some Traversals
progSubExt :: Traversal' (Program ext stmts) ext
progSubExt f = \case
    Program exts stmts -> Program <$> traverse f exts <*> pure stmts

progSubStatements :: Traversal' (Program ext stmt) (Statements stmt)
progSubStatements f = \case
    Program exts stmts -> Program exts <$> f stmts

progSubStatement :: Traversal' (Program ext stmt) stmt
progSubStatement f = \case
    Program exts stmts -> Program exts <$> traverse f stmts

stmtsSubStatement :: Traversal' (Statements stmt) stmt
stmtsSubStatement = traverse
