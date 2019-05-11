module TinyLang.Rational.Printer
    ( toStringWithIDs
    , toStringNoIDs
    ) where

import           TinyLang.Rational.Core
import           TinyLang.Var

-- | Variable names are equipped with Unique identifiers.  The
-- PrintStyle type determines whether printed variable names include
-- these or not ("x_5" versus "x").  If we're going to re-parse the
-- output of toString we probably don't want the IDs.
data PrintStyle = WithIDs | NoIDs

toStringVar :: PrintStyle -> Var -> String
toStringVar NoIDs   (Var _ name) = name
toStringVar WithIDs v            = show v   -- or explicitly tell it what to do?

toStringUnOp :: UnOp a b -> String
toStringUnOp Not  = "not "
toStringUnOp Neq0 = "neq0 "
toStringUnOp Inv  = "inv "

toStringBinOp :: BinOp a b c -> String
toStringBinOp Or  = " or "
toStringBinOp And = " and "
toStringBinOp Xor = " xor "
toStringBinOp Add = " + "
toStringBinOp Sub = " - "
toStringBinOp Mul = " * "
toStringBinOp Div = " / "

-- Do we want () round something when printing it inside some other expression?
isSimple :: Expr a -> Bool
isSimple (EVal _) = True
isSimple _        = False

-- Convert to string (with enclosing () if necessary)
toString1 :: PrintStyle -> Expr a -> String
toString1 s e = if isSimple e then toString s e else "(" ++ toString s e ++ ")"

toStringUniVal :: UniVal a -> String
toStringUniVal (UniVal Bool     b) = if b then "T" else "F"
toStringUniVal (UniVal Rational r) = show r

-- Main function
toString :: PrintStyle -> Expr a -> String
toString _ (EVal uv)            = toStringUniVal uv
toString s (EVar _ v)           = toStringVar s v
toString s (EAppUnOp op e)      = toStringUnOp op ++ toString1 s e
toString s (EAppBinOp op e1 e2) = toString1 s e1 ++ toStringBinOp op ++ toString1 s e2
toString s (EIf e e1 e2)        = "if " ++ toString1 s e ++ " then " ++ toString1 s e1 ++ " else " ++ toString1 s e2

-- | Convert an Expr to a String, ignoring Unique IDs in variable names
toStringNoIDs :: Expr a -> String
toStringNoIDs = toString NoIDs

-- | Convert an Expr to a String, including Unique IDs in variable names
toStringWithIDs :: Expr a -> String
toStringWithIDs = toString WithIDs
