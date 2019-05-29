module TinyLang.Field.Core
    ( Field (..)
    , AField (..)
    , Uni (..)
    , UniVal (..)
    , SomeUniVal (..)
    , UnOp (..)
    , BinOp (..)
    , Expr (..)
    , withGeqUni
    , VarSign (..)
    , exprVarSigns
    ) where

import           Prelude          hiding (div)
import qualified Prelude          (div)
import           TinyLang.Prelude
import           TinyLang.Var

import qualified Data.IntMap.Strict as IntMap

infixl 6 `add`, `sub`
infixl 7 `mul`, `div`

class Field f where
    zer :: f

    neg :: f -> f
    neg x = zer `sub` x

    add :: f -> f -> f

    sub :: f -> f -> f
    x `sub` y = x `add` neg y

    one :: f

    inv :: f -> f
    inv x = one `div` x

    mul :: f -> f -> f

    div :: f -> f -> f
    x `div` y = x `mul` inv y

    {-# MINIMAL zer, add, one, mul, (neg | sub), (inv | div) #-}

newtype AField f = AField
    { unAField :: f
    } deriving (Eq)

instance Field f => Field (AField f) where
    zer = coerce $ zer @f
    neg = coerce $ neg @f
    add = coerce $ add @f
    sub = coerce $ sub @f
    one = coerce $ one @f
    inv = coerce $ inv @f
    mul = coerce $ mul @f
    div = coerce $ div @f

instance Field Rational where
    zer = 0
    neg = negate
    add = (+)
    sub = (-)
    one = 1
    inv = \x -> denominator x % numerator x
    mul = (*)

instance Field f => Num (AField f) where
    negate = neg
    (+) = add
    (-) = sub
    (*) = mul

    fromInteger n = case n of
        -1            -> neg one
        0             -> zer
        1             -> one
        2             -> one `add` one
        _ | even n    -> 2 * fromInteger (n `Prelude.div` 2)
          | otherwise -> 1 + fromInteger (n - 1)

    abs    = error "no 'abs'"
    signum = error "no 'signum'"

data Uni f a where
    Bool  :: Uni f Bool
    Field :: Uni f (AField f)
    -- ^ We need this additional 'AField' wrapper in order to make 'Uni' a singleton.
    -- That is, if we made it @Field :: Uni f f@, then with @f@ instantiated to @Bool@, both
    -- @Bool@ and @Field@ would be of the same type: @Uni Bool Bool@. Since we use @Uni@ in order
    -- to reflect types at the term level, we do want it to be a singleton.
    -- Originally @Field@ didn't use the wrapper and we were getting annoying
    -- "pattern matching is not exhaustive" warnings. Now @a@ uniquely determines the constructor
    -- and we do not have such warnings.

-- Needed for the sake of deriving.
data UniVal f a = UniVal
    { _uniValUni :: Uni f a
    , _uniValVal :: a
    }

data SomeUniVal f = forall a. SomeUniVal (UniVal f a)

data UnOp f a b where
    Not  :: UnOp f Bool      Bool
    Neq0 :: UnOp f (AField f) Bool
    Neg  :: UnOp f (AField f) (AField f)
    Inv  :: UnOp f (AField f) (AField f)

data BinOp f a b c where
    Or  :: BinOp f Bool       Bool       Bool
    And :: BinOp f Bool       Bool       Bool
    Xor :: BinOp f Bool       Bool       Bool
    FEq :: BinOp f (AField f) (AField f) Bool
    Add :: BinOp f (AField f) (AField f) (AField f)
    Sub :: BinOp f (AField f) (AField f) (AField f)
    Mul :: BinOp f (AField f) (AField f) (AField f)
    Div :: BinOp f (AField f) (AField f) (AField f)

data Expr f a where
    EVal      :: UniVal f a -> Expr f a
    EVar      :: Uni f a -> Var -> Expr f a
    EIf       :: Expr f Bool -> Expr f a -> Expr f a -> Expr f a
    EAppUnOp  :: UnOp f a b -> Expr f a -> Expr f b
    EAppBinOp :: BinOp f a b c -> Expr f a -> Expr f b -> Expr f c

instance Field f => Field (Expr f (AField f)) where
    zer = EVal (UniVal Field zer)
    neg = EAppUnOp Neg
    add = EAppBinOp Add
    sub = EAppBinOp Sub
    one = EVal (UniVal Field one)
    inv = EAppUnOp Inv
    mul = EAppBinOp Mul
    div = EAppBinOp Div

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

deriving instance Show f => Show (SomeUniVal f)

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
withGeqBinOp FEq FEq y _ = y
withGeqBinOp Add Add y _ = y
withGeqBinOp Sub Sub y _ = y
withGeqBinOp Mul Mul y _ = y
withGeqBinOp Div Div y _ = y
withGeqBinOp _   _   _ z = z

-- This doesn't type check:
--
-- > UniVal uni1 x1 == UniVal uni2 x2 = withGeqUni uni1 uni2 (x1 == x2) False
--
-- because it requires the type of @x1@ and @x2@ to have an @Eq@ instance.
-- We could provide a similar to 'withGeqUni' combinator that can handle this situation,
-- but then it's easier to just pattern match on universes.
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

data VarSign f = forall a. VarSign
    { _varSignName :: String
    , _varSignUni  :: Uni f a
    }

deriving instance Show (VarSign f)

instance Eq (VarSign f) where
    VarSign name1 uni1 == VarSign name2 uni2 = withGeqUni uni1 uni2 (name1 == name2) False

exprVarSigns :: Expr f a -> IntMap (VarSign f)
exprVarSigns = go mempty where
    go :: IntMap (VarSign f) -> Expr f a -> IntMap (VarSign f)
    go names (EVal _)                            = names
    go names (EVar uni (Var (Unique uniq) name)) =
        case IntMap.lookup uniq names of
            Just sign'
                | sign == sign' -> names
                | otherwise     -> error $
                    concat ["var signature mismatch: '", show sign, "' vs '", show sign', "'"]
            Nothing -> IntMap.insert uniq sign names
        where sign = VarSign name uni
    go names (EAppUnOp _ x)                      = go names x
    go names (EAppBinOp _ x y)                   = go (go names x) y
    go names (EIf b x y)                         = go (go (go names b) x) y
