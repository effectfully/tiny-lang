module TinyLang.Field.Core
    ( module Field
    , module Var
    , module Env
    , Some (..)
    , SomeOf (..)
    , Forget (..)
    , traverseSomeOf
    , Uni (..)
    , KnownUni (..)
    , UniVal (..)
    , UniVar (..)
    , SomeUniVal
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

import           Prelude               hiding (div)
import           TinyLang.Prelude

import           Data.Field            as Field
import           TinyLang.Var          as Var
import           TinyLang.Environment  as Env
import qualified TinyLang.Boolean.Core as Boolean

data Some (f :: k -> *) = forall x. Some (f x)
data SomeOf uni (f :: k -> *) = forall x. SomeOf (uni x) (f x)

class Forget some where
    forget :: (forall x. f x -> r) -> some f -> r

instance Forget Some where
    forget f (Some a) = f a

instance Forget (SomeOf uni) where
    forget f (SomeOf _ a) = f a

traverseSomeOf :: Functor m => (forall a. f a -> m (f a)) -> SomeOf uni f -> m (SomeOf uni f)
traverseSomeOf f (SomeOf uni a) = SomeOf uni <$> f a

data Uni f a where
    Bool   :: Uni f Bool
    Field  :: Uni f (AField f)
    -- TODO: Check for Haddock post lts-13.26
    -- We need this additional 'AField' wrapper in order to make 'Uni'
    -- a singleton.  That is, if we made it @Field :: Uni f f@, then
    -- with @f@ instantiated to @Bool@, both @Bool@ and @Field@ would
    -- be of the same type: @Uni Bool Bool@. Since we use @Uni@ in
    -- order to reflect types at the term level, we do want it to be a
    -- singleton.  Originally @Field@ didn't use the wrapper and we
    -- were getting annoying "pattern matching is not exhaustive"
    -- warnings. Now @a@ uniquely determines the constructor and we do
    -- not have such warnings.
    Vector :: Uni f (Vector Bool)

class KnownUni f a where
    knownUni :: Uni f a

instance KnownUni f Bool where
    knownUni = Bool

instance f ~ f' => KnownUni f (AField f') where
    knownUni = Field

instance bool ~ Bool => KnownUni f (Vector bool) where
    knownUni = Vector

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
    EVal       :: UniVal f a -> Expr f a
    EVar       :: UniVar f a -> Expr f a
    EIf        :: Expr f Bool -> Expr f a -> Expr f a -> Expr f a
    EAppUnOp   :: UnOp f a b -> Expr f a -> Expr f b
    EAppBinOp  :: BinOp f a b c -> Expr f a -> Expr f b -> Expr f c
    EStatement :: Statement f -> Expr f a -> Expr f a

mapUniVal :: (a -> a) -> UniVal f a -> UniVal f a
mapUniVal f (UniVal uni x) = UniVal uni $ f x

zipUniVal :: (a -> a -> a) -> UniVal f a -> UniVal f a -> UniVal f a
zipUniVal f (UniVal uni x) (UniVal _ y) = UniVal uni $ f x y

mapUniValF :: Functor g => (a -> g a) -> UniVal f a -> g (UniVal f a)
mapUniValF f (UniVal uni x) = UniVal uni <$> f x

zipUniValF :: Functor g => (a -> a -> g a) -> UniVal f a -> UniVal f a -> g (UniVal f a)
zipUniValF f (UniVal uni x) (UniVal _ y) = UniVal uni <$> f x y

instance (Field f, af ~ AField f) => Field (UniVal f af) where
    zer = UniVal Field zer
    neg = mapUniVal  neg
    add = zipUniVal  add
    sub = zipUniVal  sub
    one = UniVal Field one
    inv = mapUniValF inv
    mul = zipUniVal  mul
    div = zipUniValF div

instance (Field f, af ~ AField f) => Field (Expr f af) where
    zer = EVal zer
    neg = EAppUnOp Neg
    add = EAppBinOp Add
    sub = EAppBinOp Sub
    one = EVal one
    inv = Just . EAppUnOp Inv
    mul = EAppBinOp Mul
    div = Just .* EAppBinOp Div

deriving via AField (UniVal f af) instance (Field f, af ~ AField f) => Num        (UniVal f af)
deriving via AField (UniVal f af) instance (Field f, af ~ AField f) => Fractional (UniVal f af)
deriving via AField (Expr   f af) instance (Field f, af ~ AField f) => Num        (Expr   f af)
deriving via AField (Expr   f af) instance (Field f, af ~ AField f) => Fractional (Expr   f af)

deriving instance Show (Uni f a)
deriving instance Eq   (Uni f a)

deriving instance Show (UnOp f a b)
deriving instance Eq   (UnOp f a b)

deriving instance Show (BinOp f a b c)
deriving instance Eq   (BinOp f a b c)

instance TextField f => Show (UniVal f a) where
    show (UniVal Bool   b) = "(UniVal Bool " ++ show b ++ ")"
    show (UniVal Field  i) = showField i
    show (UniVal Vector v) = "(UniVal Vector " ++ show v ++ ")"

deriving instance TextField f => Show (Statement f)
deriving instance TextField f => Show (Expr f a)

deriving instance TextField f => Show (Some (UniVal f))

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
uniOfExpr = go where
    go (EVal (UniVal uni _)) = uni
    go (EVar (UniVar uni _)) = uni
    go (EAppUnOp op _)       = withUnOpUnis op $ \_ resUni -> resUni
    go (EAppBinOp op _ _)    = withBinOpUnis op $ \_ _ resUni -> resUni
    go (EIf _ x _)           = uniOfExpr x
    go (EStatement _ expr)   = uniOfExpr expr

withGeqUni :: Uni f a1 -> Uni f a2 -> b -> (a1 ~ a2 => b) -> b
withGeqUni Bool   Bool   _ y = y
withGeqUni Field  Field  _ y = y
withGeqUni Vector Vector _ y = y
withGeqUni Bool   _ z _ = z
withGeqUni Field  _ z _ = z
withGeqUni Vector _ z _ = z

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
-- > UniVal _ x1 == UniVal _ x2 = x1 == x2
--
-- because it requires the type of @x1@ and @x2@ to have an @Eq@ instance.
-- We could provide a similar to 'withGeqUni' combinator that can handle this situation,
-- but then it's easier to just pattern match on universes.
instance Eq f => Eq (UniVal f a) where
    UniVal Bool   bool1 == UniVal Bool   bool2 = bool1 == bool2
    UniVal Field  el1   == UniVal Field  el2   = el1 == el2
    UniVal Vector vec1  == UniVal Vector vec2  = vec1 == vec2

instance Eq f => Eq (UniVar f a) where
    UniVar _ v1 == UniVar _ v2 = v1 == v2

instance Eq f => Eq (Statement f) where
    ELet (UniVar u1 v1) d1 == ELet (UniVar u2 v2) d2 =
        withGeqUni u1 u2 False $ v1 == v2 && d1 == d2
    EAssert as1            == EAssert as2            = as1 == as2

    ELet    {} == _ = False
    EAssert {} == _ = False

instance Eq f => Eq (Expr f a) where
    EVal uval1         == EVal uval2         = uval1 == uval2
    EVar uvar1         == EVar uvar2         = uvar1 == uvar2
    EIf b1 x1 y1       == EIf b2 x2 y2       = b1 == b2 && x1 == x2 && y1 == y2
    EAppUnOp o1 x1     == EAppUnOp o2 x2     = withGeqUnOp o1 o2 False $ x1 == x2
    EAppBinOp o1 x1 y1 == EAppBinOp o2 x2 y2 = withGeqBinOp o1 o2 False $ x1 == x2 && y1 == y2
    EStatement st1 e1  == EStatement st2 e2  = st1 == st2 && e1 == e2

    -- Here we explicitly pattern match on the first argument again and always return 'False'.
    -- This way we'll get a warning when an additional constructor is added to 'Expr',
    -- instead of erroneously defaulting to 'False'.
    EVal       {} == _ = False
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
    goExpr signs (EVal _) = signs
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
supplyFromAtLeastFree expr = supplyFromAtLeast . freeUniqueIntMap . unEnv $ free <> bound where
    ScopedVarSigns free bound = exprVarSigns expr

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
