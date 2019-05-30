-- | A class for fields which can be parsed, allowing us to make the parser parametric

module TinyLang.Field.ParsableField
where

import           Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import           TinyLang.Field.Core
import           TinyLang.Field.ParserUtils
import           TinyLang.Prelude           hiding (many, try)

class Field f => ParsableField f
    where parseFieldElement :: Parser f

instance ParsableField f => ParsableField (AField f)
    where parseFieldElement = AField <$> parseFieldElement

-- Parser for Rational.  Accepts things like -234 % 92981890 and also plain integers
instance ParsableField Rational where
    parseFieldElement =
        try ((%) <$>  signedDecimal <* symbol "%" <*> lexeme L.decimal)
                <|> (%1) <$> signedDecimal
        where signedDecimal = L.signed ws (lexeme L.decimal)

