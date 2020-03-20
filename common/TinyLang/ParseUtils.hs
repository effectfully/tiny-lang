-- | Some functions/types lifted out of the Parser module so that we can use them in ParsableField
-- TODO: clean up the imports/exports

{-# OPTIONS_GHC -fno-warn-orphans #-}

module TinyLang.ParseUtils
    ( Parser
    , Scope
    , MonadScope
    , Scoped (..)
    , parseBy
    , parseString
    , makeVar
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

import qualified Data.Map.Strict            as Map
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | 'Scope' maps names onto 'Var's.
type Scope = Map String Var
type MonadScope = MonadState Scope

data Scoped a = Scoped
    { _scopedScope :: Map String Var
    , _scopedValue :: a
    } deriving (Functor, Foldable, Traversable)

 -- Void -> No custom error messages
type Parser = ParsecT Void String (StateT Scope Supply)

instance (MonadSupply m, Stream s) => MonadSupply (ParsecT e s m)

parseBy :: MonadSupply m => Parser a -> String -> m (Scoped (Either String a))
parseBy parser str =
    liftSupply $
        runStateT (runParserT (top parser) "" str) mempty <&> \(errOrRes, scope) ->
            Scoped scope $ first errorBundlePretty errOrRes

parseString
    :: (ShowErrorComponent e) =>
       Parsec e String a ->
       String ->
       String ->
       Either String a
parseString parser fileName str =
    first errorBundlePretty $ runParser parser fileName str


-- | Look up a variable name. If we've already seen it, return the corresponding Var;
-- otherwise, increase the Unique counter and use it to construct a new Var.
makeVar :: (MonadSupply m, MonadState Scope m) => String -> m Var
makeVar name = do
    vars <- get
    case Map.lookup name vars of
        Just var -> pure var
        Nothing  -> do
            var <- freshVar name
            put $ Map.insert name var vars
            pure var

-- Consume whitespace
ws :: (MonadParsec e s m, Token s ~ Char) => m ()
ws = L.space space1 empty empty
-- Last two arguments are for comment delimiters.  Let's not have any comments for now.

-- Parse the whole of an input stream
top :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
top = between ws eof

-- Wrapper to consume whitespace after parsing an item using the wrapped parser
lexeme :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
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
