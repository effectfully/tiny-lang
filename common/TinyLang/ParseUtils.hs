-- | Some functions/types lifted out of the Parser module so that we can use them in ParsableField
-- TODO: clean up the imports/exports

{-# OPTIONS_GHC -fno-warn-orphans #-}

module TinyLang.ParseUtils
    ( Parser
    , Scope
    , emptyScope
    , ws
    , lexeme
    , symbol
    , parens
    , signedDecimal
    ) where

import           TinyLang.Prelude           hiding (many, try)
import           TinyLang.Var

import qualified Data.Map                   as M
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | 'Scope' maps names onto 'Var's.
type Scope = M.Map String Var

emptyScope :: Scope
emptyScope = mempty

 -- Void -> No custom error messages
type Parser = ParsecT Void String (StateT Scope Supply)

instance (MonadSupply m, Stream s) => MonadSupply (ParsecT e s m)

-- Consume whitespace
ws :: Parser ()
ws = L.space space1 empty empty
-- Last two arguments are for comment delimiters.  Let's not have any comments for now.

-- Wrapper to consume whitespace after parsing an item using the wrapped parser
lexeme :: Parser a -> Parser a
lexeme = L.lexeme ws

-- Parse a fixed string
symbol :: String -> Parser String
symbol = L.symbol ws

-- 'parens' parses something between parenthesis.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

signedDecimal :: Integral a => Parser a
signedDecimal = L.signed ws (lexeme L.decimal) <|> parens signedDecimal
