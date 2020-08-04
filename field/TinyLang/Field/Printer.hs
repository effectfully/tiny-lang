module TinyLang.Field.Printer
    ( PrintStyle (..)
    , exprToString
    , someExprToString
    , stmtToString
    , stmtsToString
    , progToString
    , Pretty (Pretty)
    ) where

import           TinyLang.Prelude

import           TinyLang.Field.Typed.Core

import qualified Data.Vector               as Vector

-- | Variable names are equipped with Unique identifiers.  The
-- PrintStyle type determines whether printed variable names include
-- these or not ("x_5" versus "x").  If we're going to re-parse the
-- output of toString we probably don't want the IDs.
data PrintStyle = WithIDs | NoIDs

toStringSomeUniVar :: PrintStyle -> SomeUniVar f -> String
toStringSomeUniVar ps = forget (toStringUniVar ps)

toStringUniVar :: PrintStyle -> UniVar f a -> String
toStringUniVar ps (UniVar uni var) = suffix uni $ toStringVar ps var where
    suffix :: Uni f a -> String -> String
    suffix Bool   = (++ " : bool")
    suffix Field  = (++ " : field")
    suffix Vector = (++ " : vector")

toStringVar :: PrintStyle -> Var -> String
toStringVar NoIDs   (Var _ name) = name
toStringVar WithIDs (Var u name) = name ++ "_" ++ show u

toStringUnOp :: UnOp f a b -> String
toStringUnOp Not  = "not "
toStringUnOp Neq0 = "neq0 "
toStringUnOp Neg  = "neg "
toStringUnOp Inv  = "inv "
toStringUnOp Unp  = "unpack "

toStringBinOp :: BinOp f a b c -> String -> String -> String
toStringBinOp Or  l r = l ++ " or "  ++ r
toStringBinOp And l r = l ++ " and " ++ r
toStringBinOp Xor l r = l ++ " xor " ++ r
toStringBinOp FEq l r = l ++ " == "  ++ r
toStringBinOp FLt l r = l ++ " < "   ++ r
toStringBinOp FLe l r = l ++ " <= "  ++ r
toStringBinOp FGe l r = l ++ " >= "  ++ r
toStringBinOp FGt l r = l ++ " > "   ++ r
toStringBinOp Add l r = l ++ " + "   ++ r
toStringBinOp Sub l r = l ++ " - "   ++ r
toStringBinOp Mul l r = l ++ " * "   ++ r
toStringBinOp Div l r = l ++ " / "   ++ r
toStringBinOp BAt l r = r ++ "[" ++ l ++ "]"

-- Do we want () round something when printing it inside some other expression?
isSimple :: Expr f a -> Bool
isSimple EConst {} = True
isSimple EVar {}   = True
isSimple _         = False

-- Convert to string (with enclosing () if necessary)
exprToString1 :: TextField f => PrintStyle -> Expr f a -> String
exprToString1 s e = if isSimple e then exprToString s e else "(" ++ exprToString s e ++ ")"

toStringBool :: Bool -> String
toStringBool b = if b then "T" else "F"

toStringUniConst :: TextField f => UniConst f a -> String
toStringUniConst (UniConst Bool   b) = toStringBool b
toStringUniConst (UniConst Field  i) = showField i
toStringUniConst (UniConst Vector v) =
    "{" ++ intercalate "," (map toStringBool $ Vector.toList v) ++ "}"

stmtToString :: TextField f => PrintStyle -> Statement f -> String
stmtToString s (ELet uniVar def) = concat
    [ "let "
    , toStringUniVar s uniVar
    , " = "
    , exprToString s def
    , ";"
    ]
stmtToString s (EAssert expr)            = "assert " ++ exprToString s expr ++ ";"

stmtsToString :: TextField f => PrintStyle -> (Statements f) -> String
stmtsToString ps = unlines . (map (stmtToString ps)) . unStatements

progToString :: TextField f => PrintStyle -> Program f -> String
progToString ps (Program exts stmts) = unlines vars ++ stmtsToString ps stmts
    where
        vars = fmap (\ext -> "ext " ++ toStringSomeUniVar ps ext ++ ";") exts

-- Main function
exprToString :: TextField f => PrintStyle -> Expr f a -> String
exprToString _ (EConst uv)          = toStringUniConst uv
exprToString s (EVar uniVar)        = "(" ++ toStringUniVar s uniVar ++ ")"
exprToString s (EAppUnOp op e)      = toStringUnOp op ++ exprToString1 s e
exprToString s (EAppBinOp op e1 e2) = toStringBinOp op (exprToString1 s e1) (exprToString1 s e2)
exprToString s (EIf e e1 e2)        = concat
    [ "if "
    , exprToString1 s e
    , " then "
    , exprToString1 s e1
    , " else "
    , exprToString1 s e2
    ]

someExprToString :: TextField f => PrintStyle -> SomeUniExpr f -> String
someExprToString s = forget $ exprToString s

newtype Pretty a = Pretty { unPretty :: a }

instance (TextField f) => Show (Pretty (Program f)) where
    show = progToString WithIDs . unPretty

-- TODO:  Impvove the pretty printer
instance (Show a) => Show (Pretty (Env a)) where
    show = show . unPretty
