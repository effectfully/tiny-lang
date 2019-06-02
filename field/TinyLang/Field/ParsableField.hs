-- | A class for fields which can be parsed, allowing us to make the parser parametric

module TinyLang.Field.ParsableField
where

import           TinyLang.Field.Core
import           TinyLang.Field.F17
import           TinyLang.Field.ParserUtils
import           TinyLang.Prelude           hiding (many, try)

import           Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L


class Field f => ParsableField f
    where parseFieldElement :: Parser f

instance ParsableField f => ParsableField (AField f)
    where parseFieldElement = AField <$> parseFieldElement

instance ParsableField F17
    where parseFieldElement = toF17 <$> lexeme L.decimal

-- Parser for Rational.  Accepts things like -234 % 92981890 and also plain integers
instance ParsableField Rational where
    parseFieldElement =
        try ((%) <$>  signedDecimal <* symbol "%" <*> lexeme L.decimal)
                <|> (%1) <$> signedDecimal
        where signedDecimal = L.signed ws (lexeme L.decimal) <|> parens (signedDecimal)
              -- We have to be a liitle careful here: you can get things like (-123) % 456
