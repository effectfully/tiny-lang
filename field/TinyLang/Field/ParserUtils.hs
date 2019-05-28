-- | Some functions/types lifted out of the Parser module so that we can use them in ParsableField
-- TODO: clean up the imports/exports

module TinyLang.Field.ParserUtils (lexeme, ws, symbol, parens, emptyIdentifierState, Parser, IdentifierState)
where

import qualified Data.Map                   as M
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           TinyLang.Prelude           hiding (many, try)
import           TinyLang.Var

-- Stuff for generating new Unique names during parsing.  Based on Name.hs in PlutusCore.
-- IdentifierState maps names onto Vars and remembers a counter for Unique IDs.
type IdentifierState = (M.Map String Var, Int)

emptyIdentifierState :: IdentifierState
emptyIdentifierState = (mempty, 0)

type Parser = ParsecT Void String (TinyLang.Prelude.State IdentifierState) -- Void -> No custom error messages

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

