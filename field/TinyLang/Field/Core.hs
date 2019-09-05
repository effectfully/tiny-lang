{-# LANGUAGE DerivingVia #-}

module TinyLang.Field.Core
    ( module Field
    , module Var
    , module Env
    , Some (..)
    , SomeOf (..)
    , forget
    , Uni (..)
    , KnownUni (..)
    , UniVal (..)
    , UniVar (..)
    , SomeUniVal
    , SomeUniVar
    , SomeUniExpr (..)
    , UnOp (..)
    , BinOp (..)
    , EConstr (..)
    , Expr (..)
    , withGeqUni
    , withKnownUni
    , VarSign (..)
    , ScopedVarSigns (..)
    , exprVarSigns
    , exprFreeVarSigns
    , embedBoolUnOp
    , embedBoolBinOp
    , embedBoolExpr
    , uniOfUnOpArg
    , uniOfUnOpRes
    , uniOfBinOpArg
    , uniOfBinOpRes
    , uniOfExpr
    ) where

import           Prelude               hiding (div)

import           Data.Field            as Field
import           TinyLang.Var          as Var
import           TinyLang.Environment  as Env
import qualified TinyLang.Boolean.Core as Boolean

data Some f = forall a. Some (f a)
data SomeOf uni f = forall a. SomeOf (uni a) (f a)

forget :: (forall a. f a -> b) -> Some f -> b
forget f (Some a) = f a

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

class KnownUni f a where
    knownUni :: Uni f a

instance KnownUni f Bool where
    knownUni = Bool

instance f ~ f' => KnownUni f (AField f') where
    knownUni = Field

-- Needed for the sake of deriving.
data UniVal f a = UniVal
    { _uniValUni :: Uni f a
    , _uniValVal :: a
    }

-- Needed for the sake of symmetry with 'UniVal'.
data UniVar f a = UniVar
    { _uniVarUni :: Uni f a
    , _uniVarVar :: Var
    } deriving (Show)

-- -- TODO: We can can unify the two above by the following data type. Should we do that?
-- data Inhabits f a b = Inhabits
--     { _inhabitsUni :: Uni f a
--     , _inhabitsVal :: b
--     }

type SomeUniVal f = Some (UniVal f)
type SomeUniVar f = Some (UniVar f)
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

-- | Things that get compiled to constraints down the pipeline.
-- The evaluation semantics is the following: each constraint becomes a check at runtime
-- and the if the check fails, we have evaluation failure. So those constraints are essentially
-- assertions.
data EConstr f
    = EConstrFEq (Expr f (AField f)) (Expr f (AField f))
    deriving (Show, Eq)

-- TODO: check that a variable is always of the same type.
data Expr f a where
    EVal      :: UniVal f a -> Expr f a
    EVar      :: UniVar f a -> Expr f a
    EIf       :: Expr f Bool -> Expr f a -> Expr f a -> Expr f a
    EAppUnOp  :: UnOp f a b -> Expr f a -> Expr f b
    EAppBinOp :: BinOp f a b c -> Expr f a -> Expr f b -> Expr f c
    ELet      :: UniVar f b -> Expr f b -> Expr f a -> Expr f a
    EConstr   :: EConstr f -> Expr f a -> Expr f a

mapUniVal :: (a -> a) -> UniVal f a -> UniVal f a
mapUniVal f (UniVal uni x) = UniVal uni $ f x

zipUniVal :: (a -> a -> a) -> UniVal f a -> UniVal f a -> UniVal f a
zipUniVal f (UniVal uni x) (UniVal _ y) = UniVal uni $ f x y

instance (Field f, af ~ AField f) => Field (UniVal f af) where
    zer = UniVal Field zer
    neg = mapUniVal neg
    add = zipUniVal add
    sub = zipUniVal sub
    one = UniVal Field one
    inv = mapUniVal inv
    mul = zipUniVal mul
    div = zipUniVal div

instance (Field f, af ~ AField f) => Field (Expr f af) where
    zer = EVal zer
    neg = EAppUnOp Neg
    add = EAppBinOp Add
    sub = EAppBinOp Sub
    one = EVal one
    inv = EAppUnOp Inv
    mul = EAppBinOp Mul
    div = EAppBinOp Div

deriving via AField (UniVal f af) instance (Field f, af ~ AField f) => Num (UniVal f af)
deriving via AField (Expr   f af) instance (Field f, af ~ AField f) => Num (Expr   f af)

deriving instance Show (Uni f a)
deriving instance Eq   (Uni f a)

deriving instance Show (UnOp f a b)
deriving instance Eq   (UnOp f a b)

deriving instance Show (BinOp f a b c)
deriving instance Eq   (BinOp f a b c)

instance TextField f => Show (UniVal f a) where
    show (UniVal Bool  b) = "(" ++ "UniVal Bool " ++ show b ++ ")"
    show (UniVal Field i) = showField i

deriving instance TextField f => Show (Expr f a)

deriving instance TextField f => Show (Some (UniVal f))

deriving instance TextField f => Show (SomeUniExpr f)

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
-- > UniVal _ x1 == UniVal _ x2 = x1 == x2
--
-- because it requires the type of @x1@ and @x2@ to have an @Eq@ instance.
-- We could provide a similar to 'withGeqUni' combinator that can handle this situation,
-- but then it's easier to just pattern match on universes.
instance Eq f => Eq (UniVal f a) where
    UniVal Bool  b1 == UniVal Bool  b2 = b1 == b2
    UniVal Field i1 == UniVal Field i2 = i1 == i2

instance Eq f => Eq (UniVar f a) where
    UniVar _ v1 == UniVar _ v2 = v1 == v2

instance Eq f => Eq (Expr f a) where
    EVal uval1                == EVal uval2                  = uval1 == uval2
    EVar uvar1                == EVar uvar2                  = uvar1 == uvar2
    EIf b1 x1 y1              == EIf b2 x2 y2                = b1 == b2 && x1 == x2 && y1 == y2
    EAppUnOp o1 x1            == EAppUnOp o2 x2              = withGeqUnOp o1 o2 (x1 == x2) False
    EAppBinOp o1 x1 y1        == EAppBinOp o2 x2 y2          =
        withGeqBinOp o1 o2 (x1 == x2 && y1 == y2) False
    ELet (UniVar u1 v1) d1 e1 == ELet (UniVar u2 v2) d2 e2   =
        withGeqUni u1 u2 (v1 == v2 && d1 == d2 && e1 == e2) False
    EConstr ec1 e1            == EConstr ec2 e2              = ec1 == ec2 && e1 == e2

    -- Here we explicitly pattern match on the first argument again and always return 'False'.
    -- This way we'll get a warning when an additional constructor is added to 'Expr',
    -- instead of erroneously defaulting to 'False'.
    EVal      {} == _ = False
    EVar      {} == _ = False
    EIf       {} == _ = False
    EAppUnOp  {} == _ = False
    EAppBinOp {} == _ = False
    ELet      {} == _ = False
    EConstr   {} == _ = False

withKnownUni :: Uni f a -> (KnownUni f a => c) -> c
withKnownUni Bool  = id
withKnownUni Field = id

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
    go signs (EVal _)               = signs
    go signs (EVar (UniVar uni (Var uniq name)))
        | tracked   = signs
        | otherwise = ScopedVarSigns (insertUnique uniq sign free) bound
        where
            ScopedVarSigns free bound = signs
            sign = VarSign name uni
            tracked = isTracked uniq sign bound || isTracked uniq sign free
    go signs (EAppUnOp _ x)         = go signs x
    go signs (EAppBinOp _ x y)      = go (go signs x) y
    go signs (EIf b x y)            = go (go (go signs b) x) y
    go signs (ELet uniVar def expr) =
        go (ScopedVarSigns free $ insertUnique uniq sign bound) expr
      where
        UniVar uni (Var uniq name) = uniVar
        sign = VarSign name uni
        ScopedVarSigns free bound = go signs def
    go signs (EConstr econstr expr) = case econstr of
        EConstrFEq lhs rhs -> go (go (go signs rhs) lhs) expr

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
    go (Boolean.EVar v)           = EVar $ UniVar Bool v
    go (Boolean.EIf b x y)        = EIf (go b) (go x) (go y)
    go (Boolean.EAppUnOp op x)    = EAppUnOp (embedBoolUnOp op) (go x)
    go (Boolean.EAppBinOp op x y) = EAppBinOp (embedBoolBinOp op) (go x) (go y)

uniOfUnOpArg :: UnOp f a b -> Uni f a
uniOfUnOpArg Not  = Bool
uniOfUnOpArg Neq0 = Field
uniOfUnOpArg Inv  = Field
uniOfUnOpArg Neg  = Field

uniOfUnOpRes :: UnOp f a b -> Uni f b
uniOfUnOpRes Not  = Bool
uniOfUnOpRes Neq0 = Bool
uniOfUnOpRes Inv  = Field
uniOfUnOpRes Neg  = Field

uniOfBinOpArg :: BinOp f a b c ->  (Uni f a, Uni f b)
uniOfBinOpArg Or  = (Bool, Bool)
uniOfBinOpArg And = (Bool, Bool)
uniOfBinOpArg Xor = (Bool, Bool)
uniOfBinOpArg FEq = (Field, Field)
uniOfBinOpArg FLt = (Field, Field)
uniOfBinOpArg FLe = (Field, Field)
uniOfBinOpArg FGe = (Field, Field)
uniOfBinOpArg FGt = (Field, Field)
uniOfBinOpArg Add = (Field, Field)
uniOfBinOpArg Sub = (Field, Field)
uniOfBinOpArg Mul = (Field, Field)
uniOfBinOpArg Div = (Field, Field)

uniOfBinOpRes :: BinOp f a b c ->  Uni f c
uniOfBinOpRes Or  = Bool
uniOfBinOpRes And = Bool
uniOfBinOpRes Xor = Bool
uniOfBinOpRes FEq = Bool
uniOfBinOpRes FLt = Bool
uniOfBinOpRes FLe = Bool
uniOfBinOpRes FGe = Bool
uniOfBinOpRes FGt = Bool
uniOfBinOpRes Add = Field
uniOfBinOpRes Sub = Field
uniOfBinOpRes Mul = Field
uniOfBinOpRes Div = Field

uniOfExpr :: Expr f a -> Uni f a
uniOfExpr = go where
    go (EVal (UniVal uni _)) = uni
    go (EVar (UniVar uni _)) = uni
    go (EAppUnOp op _)       = uniOfUnOpRes op
    go (EAppBinOp op _ _)    = uniOfBinOpRes op
    go (EIf _ x _)           = uniOfExpr x
    go (ELet _ _ expr)       = uniOfExpr expr
    go (EConstr _ expr)      = uniOfExpr expr
