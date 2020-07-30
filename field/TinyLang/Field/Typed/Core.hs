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
    , Statements
    , pattern C.Statements
    , C.unStatements
    , Program
    , pattern C.Program
    , C._programStatements
    , C._programExts
    , Expr (..)
    , withUnOpUnis
    , withBinOpUnis
    , withGeqUni
    , withKnownUni
    , VarSig (..)
    , ScopedVarSigs (..)
    , progFreeVarSigs
    , progExtVarSigs
    , progSupplyFromAtLeastFree
    , uniOfExpr
    ) where

import           Prelude                    hiding (div)
import           TinyLang.Prelude

import qualified TinyLang.Field.Core        as C
import           Data.Field                 as Field
import           TinyLang.Environment       as Env
import           TinyLang.Field.Existential
import           TinyLang.Field.Uni
import           TinyLang.Var               as Var
-- import           TinyLang.Field.Printer    (progToString, PrintStyle(..))


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


type Program    f = C.Program    (SomeUniVar f) (Statement f)
type Statements f = C.Statements                (Statement f)


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

instance Eq f => Eq (Statement f) where
    ELet (UniVar u1 v1) d1 == ELet (UniVar u2 v2) d2 =
        withGeqUni u1 u2 False $ v1 == v2 && d1 == d2
    EAssert as1 == EAssert as2 =
        as1 == as2

    ELet    {} == _ = False
    EAssert {} == _ = False

instance Eq f => Eq (Expr f a) where
    EConst uval1       == EConst uval2       = uval1 == uval2
    EVar uvar1         == EVar uvar2         = uvar1 == uvar2
    EIf b1 x1 y1       == EIf b2 x2 y2       = b1 == b2 && x1 == x2 && y1 == y2
    EAppUnOp o1 x1     == EAppUnOp o2 x2     = withGeqUnOp o1 o2 False $ x1 == x2
    EAppBinOp o1 x1 y1 == EAppBinOp o2 x2 y2 = withGeqBinOp o1 o2 False $ x1 == x2 && y1 == y2

    -- Here we explicitly pattern match on the first argument again and always return 'False'.
    -- This way we'll get a warning when an additional constructor is added to 'Expr',
    -- instead of erroneously defaulting to 'False'.
    EConst     {} == _ = False
    EVar       {} == _ = False
    EIf        {} == _ = False
    EAppUnOp   {} == _ = False
    EAppBinOp  {} == _ = False

withKnownUni :: Uni f a -> (KnownUni f a => c) -> c
withKnownUni Bool   = id
withKnownUni Field  = id
withKnownUni Vector = id

data VarSig f = forall a. VarSig
    { _varSigName :: String
    , _varSigUni  :: Uni f a
    }

deriving instance Show (VarSig f)

instance Eq (VarSig f) where
    VarSig name1 uni1 == VarSig name2 uni2 = withGeqUni uni1 uni2 False $ name1 == name2

data ScopedVarSigs f = ScopedVarSigs
    { _scopedVarSigsFree  :: Env (VarSig f)
    , _scopedVarSigsBound :: Env (VarSig f)
    } deriving (Show)

-- | Add variable to the set of bound variables
bindUniVar :: UniVar f a -> State (ScopedVarSigs f) ()
bindUniVar (UniVar uni (Var uniq name)) =
    modify $ \(ScopedVarSigs free bound) ->
        let sig    = VarSig name uni
            bound' = insertUnique uniq sig bound
        in ScopedVarSigs free bound'

-- | Add variable to the set of free variables
freeVar :: UniVar f a -> State (ScopedVarSigs f) ()
freeVar (UniVar uni (Var uniq name)) =
    modify $ \(ScopedVarSigs free bound) ->
        let sig   = VarSig name uni
            free' = insertUnique uniq sig free
        in ScopedVarSigs free' bound

-- | Check if variable is tracked in bound or free variables
isTracked :: UniVar f a -> State (ScopedVarSigs f) Bool
isTracked (UniVar uni (Var uniq name)) = do
    ScopedVarSigs free bound <- get
    pure $ isTrackedIn free || isTrackedIn bound
    where
        sig = VarSig name uni
        isTrackedIn env =
            case lookupUnique uniq env of
                Just x'
                    | x' == sig -> True
                    | otherwise -> error $ concat [ "panic: mismatch: '"
                                                  , show sig
                                                  , "' vs '"
                                                  , show x'
                                                  , "'"]
                Nothing -> False

progVS :: Program f -> State (ScopedVarSigs f) ()
progVS (C.Program exts stmts) = do
    traverse_ extVS exts
    traverse_ stmtVS stmts

extVS :: SomeUniVar f -> State (ScopedVarSigs f) ()
extVS = forget bindUniVar

-- | Gather VarSigs for a statement
stmtVS :: Statement f -> State (ScopedVarSigs f) ()
stmtVS (EAssert expr)    = exprVS expr
stmtVS (ELet uniVar def) = do
    exprVS def
    bindUniVar uniVar

-- | Gather VarSigs for an expression
exprVS :: Expr f a -> State (ScopedVarSigs f) ()
exprVS (EConst _) = pure ()
exprVS (EVar uniVar) = do
    tracked <- isTracked uniVar
    unless tracked $ freeVar uniVar
exprVS (EAppUnOp _ x) = exprVS x
exprVS (EAppBinOp _ x y) = do
    exprVS x
    exprVS y
exprVS (EIf b x y) = do
    exprVS b
    exprVS x
    exprVS y

-- | Collect all bindings
execSVS :: State (ScopedVarSigs f) () -> ScopedVarSigs f
execSVS s = execState s $ ScopedVarSigs mempty mempty

-- | Return free variable signatures for a given program
progFreeVarSigs :: Program f -> Env (VarSig f)
progFreeVarSigs = _scopedVarSigsFree . execSVS . traverse_ stmtVS

-- | Return ext variable signatures for a given program
progExtVarSigs :: Program f -> Env (VarSig f)
progExtVarSigs (C.Program exts _) = _scopedVarSigsBound . execSVS . traverse_ extVS $ exts


progSupplyFromAtLeastFree :: MonadSupply m => Program f -> m ()
progSupplyFromAtLeastFree =
    supplyFromAtLeast
    . freeUniqueIntMap
    . unEnv
    . _scopedVarSigsFree
    . execSVS
    . progVS
