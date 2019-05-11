module TinyLang.Rational.Core
    ( Uni (..)
    , UniVal (..)
    , UnOp (..)
    , BinOp (..)
    , Expr (..)
    , withGeqUni
    , exprVarNames
    ) where

import           TinyLang.Prelude
import           TinyLang.Var

import qualified Data.IntMap.Strict as IntMap

data Uni a where
    Bool     :: Uni Bool
    Rational :: Uni Rational

-- Needed for the sake of deriving.
data UniVal a = UniVal (Uni a) a

data UnOp a b where
    Not  :: UnOp Bool     Bool
    Neq0 :: UnOp Rational Bool
    Inv  :: UnOp Rational Rational

data BinOp a b c where
    Or  :: BinOp Bool     Bool     Bool
    And :: BinOp Bool     Bool     Bool
    Xor :: BinOp Bool     Bool     Bool
    Add :: BinOp Rational Rational Rational
    Sub :: BinOp Rational Rational Rational
    Mul :: BinOp Rational Rational Rational
    Div :: BinOp Rational Rational Rational

data Expr a where
    EVal      :: UniVal a -> Expr a
    EVar      :: Uni a -> Var -> Expr a
    EIf       :: Expr Bool -> Expr a -> Expr a -> Expr a
    EAppUnOp  :: UnOp a b -> Expr a -> Expr b
    EAppBinOp :: BinOp a b c -> Expr a -> Expr b -> Expr c

deriving instance Show (Uni a)
deriving instance Eq   (Uni a)

deriving instance Show (UnOp a b)
deriving instance Eq   (UnOp a b)

deriving instance Show (BinOp a b c)
deriving instance Eq   (BinOp a b c)

instance Show (UniVal a) where
    show (UniVal Bool     b) = show b
    show (UniVal Rational r) = show r

deriving instance Show (Expr a)

withGeqUni :: Uni a1 -> Uni a2 -> (a1 ~ a2 => b) -> b -> b
withGeqUni Bool     Bool     y _ = y
withGeqUni Rational Rational y _ = y
withGeqUni _       _         _ z = z

withGeqUnOp :: UnOp a1 b1 -> UnOp a2 b2 -> ((a1 ~ a2, b1 ~ b2) => d) -> d -> d
withGeqUnOp Not  Not  y _ = y
withGeqUnOp Neq0 Neq0 y _ = y
withGeqUnOp Inv  Inv  y _ = y
withGeqUnOp _    _    _ z = z

withGeqBinOp :: BinOp a1 b1 c1 -> BinOp a2 b2 c2 -> ((a1 ~ a2, b1 ~ b2, c1 ~ c2) => d) -> d -> d
withGeqBinOp Or  Or  y _ = y
withGeqBinOp And And y _ = y
withGeqBinOp Xor Xor y _ = y
withGeqBinOp Add Add y _ = y
withGeqBinOp Sub Sub y _ = y
withGeqBinOp Mul Mul y _ = y
withGeqBinOp Div Div y _ = y
withGeqBinOp _   _   _ z = z

instance Eq (UniVal a) where
    UniVal Bool     b1 == UniVal Bool     b2 = b1 == b2
    UniVal Rational r1 == UniVal Rational r2 = r1 == r2

instance Eq (Expr a) where
    EVal uv1           == EVal uv2           = uv1 == uv2
    EVar _ v1          == EVar _ v2          = v1 == v2
    EIf b1 x1 y1       == EIf b2 x2 y2       = b1 == b2 && x1 == x2 && y1 == y2
    EAppUnOp o1 x1     == EAppUnOp o2 x2     = withGeqUnOp o1 o2 (x1 == x2) False
    EAppBinOp o1 x1 y1 == EAppBinOp o2 x2 y2 = withGeqBinOp o1 o2 (x1 == x2 && y1 == y2) False
    _                  == _                  = False

exprVarNames :: Expr a -> IntMap String
exprVarNames = go mempty where
    go :: IntMap String -> Expr a -> IntMap String
    go names (EVal _)                          = names
    go names (EVar _ (Var (Unique uniq) name)) =
        case IntMap.lookup uniq names of
            Just name'
                | name == name' -> names
                | otherwise     ->
                    error $ concat ["name mismatch: '", name, "' vs '", name', "'"]
            Nothing -> IntMap.insert uniq name names
    go names (EAppUnOp _ x)                    = go names x
    go names (EAppBinOp _ x y)                 = go (go names x) y
    go names (EIf b x y)                       = go (go (go names b) x) y
