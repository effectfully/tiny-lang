-- | Some functions/types lifted out of the Parser module so that we can use them in ParsableField
-- TODO: clean up the imports/exports

{-# OPTIONS_GHC -fno-warn-orphans #-}

module TinyLang.ParseUtils
    ( parseString
    , ws
    , lexeme
    , symbol
    , parens
    , brackets
    , top
    , signedDecimal
    ) where

import           TinyLang.Prelude           hiding (many, try)
import           TinyLang.Var

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

instance (MonadSupply m, Stream s) => MonadSupply (ParsecT e s m)

parseString
    :: (MonadError String m, ShowErrorComponent e)
    => Parsec e String a
    -> String
    -> String
    -> m a
parseString parser fileName str =
    liftEither . first errorBundlePretty $ runParser parser fileName str

-- Consume whitespace
ws :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ [Char]) => m ()
ws = L.space space1 lineComment blockComment where
    -- NOTE:  Using C-family style for comments
    lineComment  = L.skipLineComment "//"
    blockComment = L.skipBlockComment "/*" "*/"

-- Parse the whole of an input stream
top :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ [Char]) => m a -> m a
top = between ws eof

-- Wrapper to consume whitespace after parsing an item using the wrapped parser
lexeme :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ [Char]) => m a -> m a
lexeme = L.lexeme ws

-- Parse a fixed string
symbol :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ [Char]) => String -> m String
symbol = L.symbol ws

-- 'parens' parses something between parenthesis.
parens :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ [Char]) => m a -> m a
parens = between (symbol "(") (symbol ")")

brackets :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ [Char]) => m a -> m a
brackets = between (symbol "[") (symbol "]")

signedDecimal :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ [Char], Integral a) => m a
signedDecimal = L.signed ws (lexeme L.decimal) <|> parens signedDecimal
