module TinyLang.Rational.Core
    ( Universe (..)
    , UnOp (..)
    , BinOp (..)
    , Expr (..)
    , withGeqUniverse
    , exprVarNames
    ) where

import           TinyLang.Prelude
import           TinyLang.Var

import qualified Data.IntMap.Strict as IntMap

data Universe a where
    Bool     :: Universe Bool
    Rational :: Universe Rational

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
    Pow :: BinOp Rational Rational Rational

data Expr a where
    EVal      :: Universe a -> a -> Expr a
    EVar      :: Universe a -> Var -> Expr a
    EIf       :: Expr Bool -> Expr a -> Expr a -> Expr a
    -- The constrains are added for the ease of deriving, they're not really needed.
    EAppUnOp  :: Show a => UnOp a b -> Expr a -> Expr b
    EAppBinOp :: (Show a, Show b) => BinOp a b c -> Expr a -> Expr b -> Expr c

deriving instance Show (Universe a)
deriving instance Eq   (Universe a)

deriving instance Show (UnOp a b)
deriving instance Eq   (UnOp a b)

deriving instance Show (BinOp a b c)
deriving instance Eq   (BinOp a b c)

-- The @Show a@ constraint is actually redundant, but it's not clear how to make deriving work
-- without it.
deriving instance Show a => Show (Expr a)

withGeqUniverse :: Universe a1 -> Universe a2 -> (a1 ~ a2 => b) -> b -> b
withGeqUniverse Bool    Bool    y _ = y
withGeqUniverse Rational Rational y _ = y
withGeqUniverse _       _       _ z = z

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
withGeqBinOp Pow Pow y _ = y
withGeqBinOp _   _   _ z = z

instance Eq (Expr a) where
    EVal Bool b1       == EVal Bool b2       = b1 == b2
    EVal Rational i1   == EVal Rational i2   = i1 == i2
    EVar _ v1          == EVar _ v2          = v1 == v2
    EIf b1 x1 y1       == EIf b2 x2 y2       = b1 == b2 && x1 == x2 && y1 == y2
    EAppUnOp o1 x1     == EAppUnOp o2 x2     = withGeqUnOp o1 o2 (x1 == x2) False
    EAppBinOp o1 x1 y1 == EAppBinOp o2 x2 y2 = withGeqBinOp o1 o2 (x1 == x2 && y1 == y2) False
    _                  == _                  = False

exprVarNames :: Expr a -> IntMap String
exprVarNames = go mempty where
    go :: IntMap String -> Expr a -> IntMap String
    go names (EVal _ _)                        = names
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
