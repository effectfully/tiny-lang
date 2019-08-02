module TinyLang.Field.Core
    ( module Field
    , Uni (..)
    , UniVal (..)
    , SomeUniVal (..)
    , SomeUniExpr (..)
    , UnOp (..)
    , BinOp (..)
    , Expr (..)
    , withGeqUni
    , boolToField
    , VarSign (..)
    , ScopedVarSigns (..)
    , exprVarSigns
    , exprFreeVarSigns
    , embedBoolUnOp
    , embedBoolBinOp
    , embedBoolExpr
    ) where

import           Prelude               hiding (div)

import           Data.Field            as Field
import           TinyLang.Var
import           TinyLang.Environment
import qualified TinyLang.Boolean.Core as Boolean

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

data SomeUniExpr f = forall a. SomeUniExpr (Uni f a) (Expr f a)

data UnOp f a b where
    Not  :: UnOp f Bool       Bool
    Neq0 :: UnOp f (AField f) Bool
    Neg  :: UnOp f (AField f) (AField f)
    Inv  :: UnOp f (AField f) (AField f)

data BinOp f a b c where
    Or  :: BinOp f Bool       Bool       Bool
    And :: BinOp f Bool       Bool       Bool
    Xor :: BinOp f Bool       Bool       Bool
    FEq :: BinOp f (AField f) (AField f) Bool
    FLt :: BinOp f (AField f) (AField f) Bool
    FLe :: BinOp f (AField f) (AField f) Bool
    FGe :: BinOp f (AField f) (AField f) Bool
    FGt :: BinOp f (AField f) (AField f) Bool
    Add :: BinOp f (AField f) (AField f) (AField f)
    Sub :: BinOp f (AField f) (AField f) (AField f)
    Mul :: BinOp f (AField f) (AField f) (AField f)
    Div :: BinOp f (AField f) (AField f) (AField f)

-- TODO: check that a variable is always of the same type.
data Expr f a where
    EVal      :: UniVal f a -> Expr f a
    EVar      :: Uni f a -> Var -> Expr f a
    EIf       :: Expr f Bool -> Expr f a -> Expr f a -> Expr f a
    EAppUnOp  :: UnOp f a b -> Expr f a -> Expr f b
    EAppBinOp :: BinOp f a b c -> Expr f a -> Expr f b -> Expr f c
    -- TODO: define @UniVar@ (in the style of @UniVal@).
    ELet      :: Uni f b -> Var -> Expr f b -> Expr f a -> Expr f a

mapUniVal :: (a -> a) -> UniVal f a -> UniVal f a
mapUniVal f (UniVal uni x) = UniVal uni $ f x

zipUniVal :: (a -> a -> a) -> UniVal f a -> UniVal f a -> UniVal f a
zipUniVal f (UniVal uni x) (UniVal _ y) = UniVal uni $ f x y

instance (Field f, f ~ f') => Field (UniVal f (AField f')) where
    zer = UniVal Field zer
    neg = mapUniVal neg
    add = zipUniVal add
    sub = zipUniVal sub
    one = UniVal Field one
    inv = mapUniVal inv
    mul = zipUniVal mul
    div = zipUniVal div

instance Num (UniVal f Bool) where
    negate = error "no 'negate'"
    (+)    = error "no '(+)'"
    (-)    = error "no '(-)'"
    (*)    = error "no '(*)'"
    abs    = error "no 'abs'"
    signum = error "no 'signum'"
    fromInteger 0 = UniVal Bool False
    fromInteger 1 = UniVal Bool True
    fromInteger n = error $ show n ++ " is not a boolean"

instance (Field f, f ~ f') => Num (UniVal f (AField f')) where
    negate = neg
    (+)    = add
    (-)    = sub
    (*)    = mul
    abs    = error "no 'abs'"
    signum = error "no 'signum'"
    fromInteger = UniVal Field . fromInteger

instance (Field f, f ~ f') => Field (Expr f (AField f')) where
    zer = EVal zer
    neg = EAppUnOp Neg
    add = EAppBinOp Add
    sub = EAppBinOp Sub
    one = EVal one
    inv = EAppUnOp Inv
    mul = EAppBinOp Mul
    div = EAppBinOp Div

deriving instance Show (Uni f a)
deriving instance Eq   (Uni f a)

deriving instance Show (UnOp f a b)
deriving instance Eq   (UnOp f a b)

deriving instance Show (BinOp f a b c)
deriving instance Eq   (BinOp f a b c)

instance Show f => Show (UniVal f a) where
    show (UniVal Bool  b) = if b then "1" else "0"
    show (UniVal Field i) = show i

deriving instance Show f => Show (Expr f a)

deriving instance Show f => Show (SomeUniVal f)

deriving instance Show f => Show (SomeUniExpr f)

withGeqUni :: Uni f a1 -> Uni f a2 -> (a1 ~ a2 => b) -> b -> b
withGeqUni Bool  Bool  y _ = y
withGeqUni Field Field y _ = y
withGeqUni _     _     _ z = z

withGeqUnOp :: UnOp f a1 b1 -> UnOp f a2 b2 -> ((a1 ~ a2, b1 ~ b2) => d) -> d -> d
withGeqUnOp Not  Not  y _ = y
withGeqUnOp Neq0 Neq0 y _ = y
withGeqUnOp Neg  Neg  y _ = y
withGeqUnOp Inv  Inv  y _ = y
withGeqUnOp _    _    _ z = z

withGeqBinOp :: BinOp f a1 b1 c1 -> BinOp f a2 b2 c2 -> ((a1 ~ a2, b1 ~ b2, c1 ~ c2) => d) -> d -> d
withGeqBinOp Or  Or  y _ = y
withGeqBinOp And And y _ = y
withGeqBinOp Xor Xor y _ = y
withGeqBinOp FEq FEq y _ = y
withGeqBinOp FLt FLt y _ = y
withGeqBinOp FLe FLe y _ = y
withGeqBinOp FGe FGe y _ = y
withGeqBinOp FGt FGt y _ = y
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

boolToField :: Field f => Bool -> f
boolToField False = zer
boolToField True  = one

data VarSign f = forall a. VarSign
    { _varSignName :: String
    , _varSignUni  :: Uni f a
    }

deriving instance Show (VarSign f)

instance Eq (VarSign f) where
    VarSign name1 uni1 == VarSign name2 uni2 = withGeqUni uni1 uni2 (name1 == name2) False

data ScopedVarSigns f = ScopedVarSigns
    { _scopedVarSignsFree  :: Env (VarSign f)
    , _scopedVarSignsBound :: Env (VarSign f)
    } deriving (Show)

isTracked :: (Eq a, Show a) => Unique -> a -> Env a -> Bool
isTracked uniq x env =
    case lookupUnique uniq env of
        Just x'
            | x == x'   -> True
            | otherwise -> error $ concat ["mismatch: '", show x, "' vs '", show x', "'"]
        Nothing -> False

-- TODO: test me somehow.
exprVarSigns :: Expr f a -> ScopedVarSigns f
exprVarSigns = go $ ScopedVarSigns mempty mempty where
    go :: ScopedVarSigns f -> Expr f a -> ScopedVarSigns f
    go signs (EVal _)                            = signs
    go signs (EVar uni (Var uniq name))
        | tracked   = signs
        | otherwise = ScopedVarSigns (insertUnique uniq sign free) bound
        where
            ScopedVarSigns free bound = signs
            sign = VarSign name uni
            tracked = isTracked uniq sign bound || isTracked uniq sign free
    go signs (EAppUnOp _ x)                      = go signs x
    go signs (EAppBinOp _ x y)                   = go (go signs x) y
    go signs (EIf b x y)                         = go (go (go signs b) x) y
    go signs (ELet uni (Var uniq name) def expr) =
        go (ScopedVarSigns free $ insertUnique uniq sign bound) expr
      where
        ScopedVarSigns free bound = go signs def
        sign = VarSign name uni

exprFreeVarSigns :: Expr f a -> Env (VarSign f)
exprFreeVarSigns = _scopedVarSignsFree . exprVarSigns

embedBoolUnOp :: Boolean.UnOp -> UnOp f Bool Bool
embedBoolUnOp Boolean.Not = Not

embedBoolBinOp :: Boolean.BinOp -> BinOp f Bool Bool Bool
embedBoolBinOp Boolean.Or  = Or
embedBoolBinOp Boolean.And = And
embedBoolBinOp Boolean.Xor = Xor

embedBoolExpr :: Boolean.Expr -> Expr f Bool
embedBoolExpr = go where
    go (Boolean.EVal b)           = EVal $ UniVal Bool b
    go (Boolean.EVar v)           = EVar Bool v
    go (Boolean.EIf b x y)        = EIf (go b) (go x) (go y)
    go (Boolean.EAppUnOp op x)    = EAppUnOp (embedBoolUnOp op) (go x)
    go (Boolean.EAppBinOp op x y) = EAppBinOp (embedBoolBinOp op) (go x) (go y)
