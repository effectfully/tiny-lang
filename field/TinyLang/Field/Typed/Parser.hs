{-# OPTIONS_GHC -fno-warn-orphans #-}

{-| Typed Parser.

This module exposes the old API of the Typed Parser.

For the new API please refer to "TinyLang.Field.Raw.Parser".

-}
module TinyLang.Field.Typed.Parser
    ( parseProgram
    , parseProgramFrom
    ) where

import           TinyLang.Prelude                 hiding (many, option, some,
                                                   try)

import           Data.Field
import           TinyLang.Field.Raw.Parser
import           TinyLang.Field.Typed.Core
import           TinyLang.Field.Typed.TypeChecker
import           TinyLang.ParseUtils

instance TextField f => IsString (Program f) where
    fromString = either error id . runSupplyT . parseProgram

-- | Parse a @String@ and return @Either@ an error message or an @Program@ of some type.
-- If the result is an error, then return the latest 'Scope', otherwise return the 'Scope'
-- consisting of all free variables of the expression.

parseProgramFrom
    :: forall f m. (MonadError String m, MonadSupply m, TextField f)
    => String -> String -> m (Program f)
parseProgramFrom fileName str = do
    progRaw <- parseString (pTop @f) fileName str
    typeProgram progRaw


-- | Convenience version of @parseScopedProgram'@ with an empty file name.
parseProgram
    :: forall f m. (MonadError String m, MonadSupply m, TextField f)
    => String -> m (Program f)
parseProgram = parseProgramFrom ""
