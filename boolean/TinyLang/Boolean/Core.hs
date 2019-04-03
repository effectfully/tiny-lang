module TinyLang.Boolean.Core
    ( UnOp (..)
    , BinOp (..)
    , Expr (..)
    ) where

import           TinyLang.Var

data UnOp
    = Not

data BinOp
    = Or
    | And
    | Xor

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



