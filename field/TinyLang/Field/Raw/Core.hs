{-| A Raw (untyped) AST
-}
module TinyLang.Field.Raw.Core
    ( Identifier
    , Var(..)
    , Expr(..)
    , BinOp(..)
    , UnOp(..)
    , Statement(..)
    , Program
    , pattern Program
    , C._programStatements
    , C._programExts
    , Statements
    , pattern Statements
    , C.unStatements
    , RawProgram
    , RawStatements
    , RawStatement
    , RawExpr
    ) where

import qualified TinyLang.Field.Uni      as U
import qualified TinyLang.Field.Type     as T
import qualified TinyLang.Field.Core     as C

import           GHC.Generics
import           Quiet

{-| = AST
-}
type Identifier = String

newtype Var = Var { unVar :: Identifier }
    deriving (Eq, Generic, Ord)
    deriving (Show) via (Quiet Var)

{-| In our AST we have the following

* @Program v f@,

* @Statements v f@,

* @Statement v f@ and

At the moment @Program v f@ is a newtype wrapper around @Statements v f@, and
@Statements v f@ is a newtype wrapper around @[Statement v f]@.

We specify those wrappers as some operations work on the program level, some
operations work on the statements level, and some operations work on the
statement level; the operations acting on statement level are not necessarily
mappable over a list of statements.
-}

type Program v f = C.Program (v, T.Type f) (Statement v f)
pattern Program :: [(v, T.Type f)] -> Statements v f -> Program v f
pattern Program exts stmts = C.Program exts stmts

type Statements v f = C.Statements (Statement v f)
pattern Statements :: [Statement v f] -> Statements v f
pattern Statements stmts = C.Statements stmts

data Statement v f
    = ELet    (v, T.Type f) (Expr v f)
    | EAssert (Expr v f)
    | EFor    v              Integer    Integer (Statements v f)
    deriving (Show)

data Expr v f
    = EConst     (U.SomeUniConst f)
    | EVar       v
    | EAppBinOp  BinOp              (Expr v f) (Expr v f)
    | EAppUnOp   UnOp               (Expr v f)
    | EIf        (Expr v f)         (Expr v f) (Expr v f)
    | ETypeAnn   (T.Type f)         (Expr v f)
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

{-| = Utility Type Aliases
-}
type RawProgram    f = Program    Var f
type RawStatements f = Statements Var f
type RawStatement  f = Statement  Var f
type RawExpr       f = Expr       Var f
