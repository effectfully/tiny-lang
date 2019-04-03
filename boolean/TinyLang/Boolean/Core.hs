module TinyLang.Boolean.Core
    ( UnOp (..)
    , BinOp (..)
    , Expr (..)
    ) where

import           TinyLang.Prelude
import           TinyLang.Var

data UnOp
    = Not
    deriving (Generic)

data BinOp
    = Or
    | And
    | Xor
    deriving (Generic)

-- TODO:
-- 1. pretty-printing
-- 2. parsing
-- 3. evaluation
-- 4. generation of arbitrary expressions
data Expr
    = EVal Bool
    | EVar Var
    | EIf Expr Expr Expr
    | EAppUnOp UnOp Expr
    | EAppBinOp BinOp Expr Expr
    deriving (Generic)

instance Monad m => Serial m UnOp
instance Monad m => Serial m BinOp
instance Monad m => Serial m Expr
