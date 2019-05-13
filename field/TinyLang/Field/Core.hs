module TinyLang.Field.Core
    ( Field (..)
    , AField (..)
    , Uni (..)
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

class Field f where
    zer :: f
    neg :: f -> f
    add :: f -> f -> f
    sub :: f -> f -> f
    one :: f
    inv :: f -> f
    mul :: f -> f -> f
    div :: f -> f -> f

newtype AField f = AField
    { unAField :: f
    } deriving (Eq)

data Uni f a where
    Bool  :: Uni f Bool
    Field :: Uni f (AField f)

-- Needed for the sake of deriving.
data UniVal f a = UniVal (Uni f a) a

data UnOp f a b where
    Not  :: UnOp f Bool Bool
    Neq0 :: UnOp f f    Bool
    Inv  :: UnOp f f    f

data BinOp f a b c where
    Or  :: BinOp f Bool Bool Bool
    And :: BinOp f Bool Bool Bool
    Xor :: BinOp f Bool Bool Bool
    Add :: BinOp f f    f    f
    Sub :: BinOp f f    f    f
    Mul :: BinOp f f    f    f
    Div :: BinOp f f    f    f

data Expr f a where
    EVal      :: UniVal f a -> Expr f a
    EVar      :: Uni f a -> Var -> Expr f a
    EIf       :: Expr f Bool -> Expr f a -> Expr f a -> Expr f a
    EAppUnOp  :: UnOp f a b -> Expr f a -> Expr f b
    EAppBinOp :: BinOp f a b c -> Expr f a -> Expr f b -> Expr f c

deriving instance Show (Uni f a)
deriving instance Eq   (Uni f a)

deriving instance Show (UnOp f a b)
deriving instance Eq   (UnOp f a b)

deriving instance Show (BinOp f a b c)
deriving instance Eq   (BinOp f a b c)

instance Show f => Show (AField f) where
    show = show . unAField

instance Show f => Show (UniVal f a) where
    show (UniVal Bool  b) = show b
    show (UniVal Field i) = show i

deriving instance Show f => Show (Expr f a)

withGeqUni :: Uni f a1 -> Uni f a2 -> (a1 ~ a2 => b) -> b -> b
withGeqUni Bool  Bool  y _ = y
withGeqUni Field Field y _ = y
withGeqUni _     _     _ z = z

withGeqUnOp :: UnOp f a1 b1 -> UnOp f a2 b2 -> ((a1 ~ a2, b1 ~ b2) => d) -> d -> d
withGeqUnOp Not  Not  y _ = y
withGeqUnOp Neq0 Neq0 y _ = y
withGeqUnOp Inv  Inv  y _ = y
withGeqUnOp _    _    _ z = z

withGeqBinOp :: BinOp f a1 b1 c1 -> BinOp f a2 b2 c2 -> ((a1 ~ a2, b1 ~ b2, c1 ~ c2) => d) -> d -> d
withGeqBinOp Or  Or  y _ = y
withGeqBinOp And And y _ = y
withGeqBinOp Xor Xor y _ = y
withGeqBinOp Add Add y _ = y
withGeqBinOp Sub Sub y _ = y
withGeqBinOp Mul Mul y _ = y
withGeqBinOp Div Div y _ = y
withGeqBinOp _   _   _ z = z

instance Eq f => Eq (UniVal f a) where
    UniVal Bool  b1 == UniVal Bool  b2 = b1 == b2
    UniVal Field i1 == UniVal Field i2 = i1 == i2

instance Eq f => Eq (Expr f a) where
    EVal uv1           == EVal uv2           = uv1 == uv2
    EVar _ v1          == EVar _ v2          = v1 == v2
    EIf b1 x1 y1       == EIf b2 x2 y2       = b1 == b2 && x1 == x2 && y1 == y2
    EAppUnOp o1 x1     == EAppUnOp o2 x2     = withGeqUnOp o1 o2 (x1 == x2) False
    EAppBinOp o1 x1 y1 == EAppBinOp o2 x2 y2 = withGeqBinOp o1 o2 (x1 == x2 && y1 == y2) False
    _                  == _                  = False

exprVarNames :: Expr f a -> IntMap String
exprVarNames = go mempty where
    go :: IntMap String -> Expr f a -> IntMap String
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
