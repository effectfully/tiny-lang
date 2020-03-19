{-| A Raw (untyped) AST
-}
module TinyLang.Field.Raw.Core
    ( Identifier
    , Var(..)
    , Expr(..)
    , BinOp(..)
    , UnOp(..)
    , Statement(..)
    , RawExpr
    , RawStatement
    ) where

import TinyLang.Field.UniConst

{-| = AST
-}
type Identifier = String

newtype Var = Var Identifier
    deriving (Eq, Show)

{-| @Expr v f@ is parameterised by the type of variable @v@.
-}
data Expr v f
    = EConst     (SomeUniConst f)
    | EVar       v
    | EAppBinOp  BinOp           (Expr v f) (Expr v f)
    | EAppUnOp   UnOp            (Expr v f)
    | EStatement (Statement v f) (Expr v f)
    | EIf        (Expr v f)      (Expr v f) (Expr v f)
    deriving (Show)

data BinOp
    = Or
    | And
    | Xor
    | FEq
    | FLe
    | FLt
    | FGe
    | FGt
    | Add
    | Sub
    | Mul
    | Div
    | BAt
    deriving (Show)

data UnOp
    = Not
    | Neq0
    | Neg
    | Inv
    | Unp
    deriving (Show)

data Statement v f
    = ELet    v          (Expr v f)
    | EAssert (Expr v f)
    | EFor    v          (Expr v f) (Expr v f) [Statement v f]
    deriving (Show)

{-| = Utility Type Aliases
-}
type RawExpr = Expr Var
type RawStatement = Statement Var

