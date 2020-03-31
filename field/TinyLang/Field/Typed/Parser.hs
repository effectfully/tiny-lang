{-# OPTIONS_GHC -fno-warn-orphans #-}

{-| Typed Parser.

This module exposes the old API of the Typed Parser.

For the new API please refer to "TinyLang.Field.Raw.Parser".

-}
module TinyLang.Field.Typed.Parser
    ( parseBy
    , parseScopedExpr
    , parseExpr
    ) where

import           TinyLang.Prelude               hiding (many, some, try, option)

import           Data.Field
import           TinyLang.Field.Raw.Parser
import           TinyLang.Field.Typed.Core
import           TinyLang.Field.Typed.TypeChecker
import           TinyLang.Field.Rename
import           TinyLang.ParseUtils

import qualified Data.IntMap.Strict             as IntMap
import qualified Data.IntSet                    as IntSet
import qualified Data.Map.Strict                as Map

-- TODO: use a proper @newtype@.
instance TextField f => IsString (Scoped (Some (Expr f))) where
    fromString = fmap (either error $ forget Some) . runSupply . parseScopedExpr

instance TextField f => IsString (Some (Expr f)) where
    fromString = _scopedValue <$> fromString

-- | Parse a @String@ and return @Either@ an error message or an @Expr@ of some type.
-- If the result is an error, then return the latest 'Scope', otherwise return the 'Scope'
-- consisting of all free variables of the expression.
parseScopedExpr
    :: forall f m. (MonadSupply m, TextField f)
    => String -> m (Scoped (Either String (SomeUniExpr f)))
parseScopedExpr str = do
    Scoped totalScope errorOrSomeUniExpr <- parseBy (pTop @f) str
    case errorOrSomeUniExpr of
        Left err -> return . Scoped totalScope $ Left err
        Right rawExpr -> do
            typed <- runExceptT (typeCheck rawExpr)
            case typed of
                Left err' -> return . Scoped totalScope $ Left err'
                Right (SomeOf uni e) -> do
                    eRen <- renameExpr e
                    let freeIndices = IntMap.keysSet . unEnv $ exprFreeVarSigns eRen
                        isFree var = unUnique (_varUniq var) `IntSet.member` freeIndices
                        freeScope = Map.filter isFree totalScope
                    return . Scoped freeScope . Right $ SomeOf uni eRen
                    

-- | Parse a @String@ and return @Either@ an error message or an @Expr@ of some type.
parseExpr
    :: forall f m. (MonadSupply m, TextField f)
    => String -> m (Either String (SomeUniExpr f))
parseExpr = fmap _scopedValue . parseScopedExpr
