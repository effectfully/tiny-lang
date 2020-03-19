module TinyLang.Boolean.Core
    ( UnOp (..)
    , BinOp (..)
    , Expr (..)
    , exprVarNames
    ) where

import           TinyLang.Prelude
import           TinyLang.Var

import qualified Data.IntMap.Strict as IntMap

data UnOp
    = Not
    deriving (Show, Read, Eq)

data BinOp
    = Or
    | And
    | Xor
    deriving (Show, Read, Eq)

data Expr
    = EConst Bool
    | EVar Var
    | EIf Expr Expr Expr
    | EAppUnOp UnOp Expr
    | EAppBinOp BinOp Expr Expr
    deriving (Show, Eq)

exprVarNames :: Expr -> IntMap String
exprVarNames = go mempty where
    go names (EConst _)                      = names
    go names (EVar (Var (Unique uniq) name)) =
        case IntMap.lookup uniq names of
            Just name'
                | name == name' -> names
                | otherwise     ->
                    error $ concat ["name mismatch: '", name, "' vs '", name', "'"]
            Nothing -> IntMap.insert uniq name names
    go names (EAppUnOp _ x)                  = go names x
    go names (EAppBinOp _ x y)               = foldl' go names [x, y]
    go names (EIf b x y)                     = foldl' go names [b, x, y]
