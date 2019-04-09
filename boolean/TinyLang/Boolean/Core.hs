module TinyLang.Boolean.Core
    ( UnOp (..)
    , BinOp (..)
    , Expr (..)
    , exprVarNames
    ) where

import           TinyLang.Prelude
import           TinyLang.Var

import qualified Data.IntMap.Strict    as IntMap

data UnOp
    = Not
    deriving (Show, Read, Generic, Eq)

data BinOp
    = Or
    | And
    | Xor
    deriving (Show, Read, Generic, Eq)

data Expr
    = EVal Bool
    | EVar Var
    | EIf Expr Expr Expr
    | EAppUnOp UnOp Expr
    | EAppBinOp BinOp Expr Expr
    deriving (Show, Generic, Eq)

instance Monad m => Serial m UnOp
instance Monad m => Serial m BinOp
instance Monad m => Serial m Expr

exprVarNames :: Expr -> IntMap String
exprVarNames = go mempty where
    go names (EVal _)                        = names
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
