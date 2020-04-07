module TinyLang.Field.Typed.Core
    ( module Field
    , module Var
    , module Env
    , Some (..)
    , SomeOf (..)
    , Forget (..)
    , traverseSomeOf
    , Uni (..)
    , KnownUni (..)
    , UniConst (..)
    , UniVar (..)
    , SomeUniConst
    , SomeUniVar
    , SomeUniExpr
    , UnOp (..)
    , BinOp (..)
    , Statement (..)
    , Expr (..)
    , withUnOpUnis
    , withBinOpUnis
    , withGeqUni
    , withKnownUni
    , VarSign (..)
    , ScopedVarSigns (..)
    , exprVarSigns
    , exprFreeVarSigns
    , supplyFromAtLeastFree
    , embedBoolUnOp
    , embedBoolBinOp
    , embedBoolExpr
    , uniOfExpr
    ) where

import           Prelude                    hiding (div)
import           TinyLang.Prelude

import           Data.Field                 as Field
import qualified TinyLang.Boolean.Core      as Boolean
import           TinyLang.Environment       as Env
import           TinyLang.Field.Existential
import           TinyLang.Field.UniConst
import           TinyLang.Var               as Var


-- Needed for the sake of symmetry with 'UniConst'.
data UniVar f a = UniVar
    { _uniVarUni :: Uni f a
    , _uniVarVar :: Var
    } deriving (Show)

-- -- TODO: We can can unify the two above by the following data type. Should we do that?
-- data Inhabits f a b = Inhabits
--     { _inhabitsUni :: Uni f a
--     , _inhabitsVal :: b
--     }

type SomeUniVar f = Some (UniVar f)
type SomeUniExpr f = SomeOf (Uni f) (Expr f)

data UnOp f a b where
    Not  :: UnOp f Bool       Bool
    Neq0 :: UnOp f (AField f) Bool
    Neg  :: UnOp f (AField f) (AField f)
    Inv  :: UnOp f (AField f) (AField f)
    Unp  :: UnOp f (AField f) (Vector Bool)

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
    BAt :: BinOp f (AField f) (Vector Bool) Bool

data Statement f where
    ELet    :: UniVar f a -> Expr f a -> Statement f
    -- | Things that get compiled to constraints down the pipeline.
    -- The evaluation semantics is the following: each assertion becomes a check at runtime
    -- and the if the check fails, we have evaluation failure.
    EAssert :: Expr f Bool -> Statement f

data Expr f a where
    EConst     :: UniConst f a -> Expr f a
    EVar       :: UniVar f a -> Expr f a
    EIf        :: Expr f Bool -> Expr f a -> Expr f a -> Expr f a
    EAppUnOp   :: UnOp f a b -> Expr f a -> Expr f b
    EAppBinOp  :: BinOp f a b c -> Expr f a -> Expr f b -> Expr f c
    EStatement :: Statement f -> Expr f a -> Expr f a

instance (Field f, af ~ AField f) => Field (Expr f af) where
    zer = EConst zer
    neg = EAppUnOp Neg
    add = EAppBinOp Add
    sub = EAppBinOp Sub
    one = EConst one
    inv = Just . EAppUnOp Inv
    mul = EAppBinOp Mul
    div = Just .* EAppBinOp Div

deriving via AField (Expr   f af) instance (Field f, af ~ AField f) => Num        (Expr   f af)
deriving via AField (Expr   f af) instance (Field f, af ~ AField f) => Fractional (Expr   f af)

deriving instance Show (UnOp f a b)
deriving instance Eq   (UnOp f a b)

deriving instance Show (BinOp f a b c)
deriving instance Eq   (BinOp f a b c)

deriving instance TextField f => Show (Statement f)
deriving instance TextField f => Show (Expr f a)


deriving instance TextField f => Show (SomeUniExpr f)

withUnOpUnis :: UnOp f a b -> (Uni f a -> Uni f b -> c) -> c
withUnOpUnis Not  k = k knownUni knownUni
withUnOpUnis Neq0 k = k knownUni knownUni
withUnOpUnis Inv  k = k knownUni knownUni
withUnOpUnis Neg  k = k knownUni knownUni
withUnOpUnis Unp  k = k knownUni knownUni

withBinOpUnis :: BinOp f a b c -> (Uni f a -> Uni f b -> Uni f c -> d) -> d
withBinOpUnis Or  k = k knownUni knownUni knownUni
withBinOpUnis And k = k knownUni knownUni knownUni
withBinOpUnis Xor k = k knownUni knownUni knownUni
withBinOpUnis FEq k = k knownUni knownUni knownUni
withBinOpUnis FLt k = k knownUni knownUni knownUni
withBinOpUnis FLe k = k knownUni knownUni knownUni
withBinOpUnis FGe k = k knownUni knownUni knownUni
withBinOpUnis FGt k = k knownUni knownUni knownUni
withBinOpUnis Add k = k knownUni knownUni knownUni
withBinOpUnis Sub k = k knownUni knownUni knownUni
withBinOpUnis Mul k = k knownUni knownUni knownUni
withBinOpUnis Div k = k knownUni knownUni knownUni
withBinOpUnis BAt k = k knownUni knownUni knownUni

uniOfExpr :: Expr f a -> Uni f a
uniOfExpr (EConst (UniConst uni _)) = uni
uniOfExpr (EVar (UniVar uni _))     = uni
uniOfExpr (EAppUnOp op _)           = withUnOpUnis op $ \_ resUni -> resUni
uniOfExpr (EAppBinOp op _ _)        = withBinOpUnis op $ \_ _ resUni -> resUni
uniOfExpr (EIf _ x _)               = uniOfExpr x
uniOfExpr (EStatement _ expr)       = uniOfExpr expr

withGeqUnOp :: UnOp f a1 b1 -> UnOp f a2 b2 -> d -> ((a1 ~ a2, b1 ~ b2) => d) -> d
withGeqUnOp unOp1 unOp2 z y =
    withUnOpUnis unOp1 $ \argUni1 resUni1 ->
    withUnOpUnis unOp2 $ \argUni2 resUni2 ->
    withGeqUni argUni1 argUni2 z $
    withGeqUni resUni1 resUni2 z $
        if unOp1 /= unOp2 then z else y

withGeqBinOp :: BinOp f a1 b1 c1 -> BinOp f a2 b2 c2 -> d -> ((a1 ~ a2, b1 ~ b2, c1 ~ c2) => d) -> d
withGeqBinOp binOp1 binOp2 z y =
    withBinOpUnis binOp1 $ \argUni11 argUni12 resUni1 ->
    withBinOpUnis binOp2 $ \argUni21 argUni22 resUni2 ->
    withGeqUni argUni11 argUni21 z $
    withGeqUni argUni12 argUni22 z $
    withGeqUni resUni1  resUni2  z $
        if binOp1 /= binOp2 then z else y

-- This doesn't type check:
--
-- > UniConst _ x1 == UniConst _ x2 = x1 == x2
--
-- because it requires the type of @x1@ and @x2@ to have an @Eq@ instance.
-- We could provide a similar to 'withGeqUni' combinator that can handle this situation,
-- but then it's easier to just pattern match on universes.
instance Eq f => Eq (UniVar f a) where
    UniVar _ v1 == UniVar _ v2 = v1 == v2

instance Eq f => Eq (Statement f) where
    ELet (UniVar u1 v1) d1 == ELet (UniVar u2 v2) d2 =
        withGeqUni u1 u2 False $ v1 == v2 && d1 == d2
    EAssert as1            == EAssert as2            = as1 == as2

    ELet    {} == _ = False
    EAssert {} == _ = False

instance Eq f => Eq (Expr f a) where
    EConst uval1       == EConst uval2         = uval1 == uval2
    EVar uvar1         == EVar uvar2         = uvar1 == uvar2
    EIf b1 x1 y1       == EIf b2 x2 y2       = b1 == b2 && x1 == x2 && y1 == y2
    EAppUnOp o1 x1     == EAppUnOp o2 x2     = withGeqUnOp o1 o2 False $ x1 == x2
    EAppBinOp o1 x1 y1 == EAppBinOp o2 x2 y2 = withGeqBinOp o1 o2 False $ x1 == x2 && y1 == y2
    EStatement st1 e1  == EStatement st2 e2  = st1 == st2 && e1 == e2

    -- Here we explicitly pattern match on the first argument again and always return 'False'.
    -- This way we'll get a warning when an additional constructor is added to 'Expr',
    -- instead of erroneously defaulting to 'False'.
    EConst     {} == _ = False
    EVar       {} == _ = False
    EIf        {} == _ = False
    EAppUnOp   {} == _ = False
    EAppBinOp  {} == _ = False
    EStatement {} == _ = False

withKnownUni :: Uni f a -> (KnownUni f a => c) -> c
withKnownUni Bool   = id
withKnownUni Field  = id
withKnownUni Vector = id

data VarSign f = forall a. VarSign
    { _varSignName :: String
    , _varSignUni  :: Uni f a
    }

deriving instance Show (VarSign f)

instance Eq (VarSign f) where
    VarSign name1 uni1 == VarSign name2 uni2 = withGeqUni uni1 uni2 False $ name1 == name2

data ScopedVarSigns f = ScopedVarSigns
    { _scopedVarSignsFree  :: Env (VarSign f)
    , _scopedVarSignsBound :: Env (VarSign f)
    } deriving (Show)

isTracked :: (Eq a, Show a) => Unique -> a -> Env a -> Bool
isTracked uniq x env =
    case lookupUnique uniq env of
        Just x'
            | x == x'   -> True
            | otherwise -> error $ concat ["panic: mismatch: '", show x, "' vs '", show x', "'"]
        Nothing -> False

-- TODO: test me somehow.
exprVarSigns :: Expr f a -> ScopedVarSigns f
exprVarSigns = goExpr $ ScopedVarSigns mempty mempty where
    goStat :: ScopedVarSigns f -> Statement f -> ScopedVarSigns f
    goStat signs (ELet uniVar def) = ScopedVarSigns free $ insertUnique uniq sign bound where
        UniVar uni (Var uniq name) = uniVar
        sign = VarSign name uni
        ScopedVarSigns free bound = goExpr signs def
    goStat signs (EAssert expr) = goExpr signs expr

    goExpr :: ScopedVarSigns f -> Expr f a -> ScopedVarSigns f
    goExpr signs (EConst _) = signs
    goExpr signs (EVar (UniVar uni (Var uniq name)))
        | tracked   = signs
        | otherwise = ScopedVarSigns (insertUnique uniq sign free) bound
        where
            ScopedVarSigns free bound = signs
            sign = VarSign name uni
            tracked = isTracked uniq sign bound || isTracked uniq sign free
    goExpr signs (EAppUnOp _ x) = goExpr signs x
    goExpr signs (EAppBinOp _ x y) = goExpr (goExpr signs x) y
    goExpr signs (EIf b x y) = goExpr (goExpr (goExpr signs b) x) y
    goExpr signs (EStatement st e) = goExpr (goStat signs st) e

exprFreeVarSigns :: Expr f a -> Env (VarSign f)
exprFreeVarSigns = _scopedVarSignsFree . exprVarSigns

supplyFromAtLeastFree :: MonadSupply m => Expr f a -> m ()
supplyFromAtLeastFree =
    supplyFromAtLeast . freeUniqueIntMap . unEnv . _scopedVarSignsFree . exprVarSigns


embedBoolUnOp :: Boolean.UnOp -> UnOp f Bool Bool
embedBoolUnOp Boolean.Not = Not

embedBoolBinOp :: Boolean.BinOp -> BinOp f Bool Bool Bool
embedBoolBinOp Boolean.Or  = Or
embedBoolBinOp Boolean.And = And
embedBoolBinOp Boolean.Xor = Xor

embedBoolExpr :: Boolean.Expr -> Expr f Bool
embedBoolExpr = go where
    go (Boolean.EConst b)         = EConst $ UniConst Bool b
    go (Boolean.EVar v)           = EVar $ UniVar Bool v
    go (Boolean.EIf b x y)        = EIf (go b) (go x) (go y)
    go (Boolean.EAppUnOp op x)    = EAppUnOp (embedBoolUnOp op) (go x)
    go (Boolean.EAppBinOp op x y) = EAppBinOp (embedBoolBinOp op) (go x) (go y)
