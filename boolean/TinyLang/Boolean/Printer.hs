module TinyLang.Boolean.Printer (toString)
where

import           TinyLang.Boolean.Core
import           TinyLang.Boolean.Environment (lookupVar)
import           TinyLang.Var

-- Do we want () round something when printing it inside some other expression?
isSimple :: Expr -> Bool
isSimple (EVal _) = True
isSimple (EVar _) = True
isSimple _        = False

toStringUnOp :: UnOp -> String
toStringUnOp Not = "not "

toStringBinOp :: BinOp -> String
toStringBinOp Or  = " or "
toStringBinOp And = " and "
toStringBinOp Xor = " xor "

-- Convert to string (with enclosing () if necessary)
toString1 :: Expr -> String
toString1 e = if isSimple e then toString e else "(" ++ toString e ++ ")"

toString :: Expr -> String
toString (EVal b) = if b then "T" else "F"
toString (EVar v) = _varName v
toString (EIf e e1 e2) = "if " ++ toString1 e ++ " then " ++ toString1 e1 ++ " else " ++ toString1 e2
toString (EAppUnOp op e) = toStringUnOp op ++ toString1 e
toString (EAppBinOp op e1 e2) = toString1 e1 ++ toStringBinOp op ++ toString1 e2

