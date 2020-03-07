{-|
= Untyped Parser

A simple parser for language involving booleans, field elements, and
vectors.

Given that the language is simple enough we are mixing lexical
analysis and grammar a bit.  To avoid any ambiguities between lexical
analysis and grammar we will explicitly mark all lexical tokens.

Please note that expressions are the designated start production for
the parser.

We are using the following convention

* /tokens/ are enclosed in parentheses @""@.
* /regular expressions/ are enclosed in square brackets @[]@.

== Literals

Literals are defined as follows

@
bool-literal ::= "T" | "F"
vec-literal  ::= "{" (bool-literal ("," bool-literal)*)? "}"
int-literal  ::= [0-9]+
@

== Keywords

Here is a list of keywords used in the language.

@
keyword ::=
  "and"
  "assert"
  "do"
  "else"
  "end"
  "for"
  "if"
  "inv"
  "let"
  "neq0"
  "not"
  "or"
  "then"
  "to"
  "unpack"
  "xor"
@

== Identifiers

To avoid a syntactic clash with /bool-literals/, identifiers for field
variables start with a lower-case letter.  Identifiers for boolean
variables are prefiex by @?@ and identifiers for vector variables are
prefixed by @#@.  We will check if an identifier is not a keyword.

@
ident ::= ( "?" | "#" ) [a-z] ([a-z0-9_'])*
@

== Constants

At the moment constants consist of literals only.

@
const ::=
  bool-literal
  int-literal
  vec-literal
@

== Expressions

As mentioned above @expr@ is the designated start production for the
parser.

@
expr ::=
  const
  "(" expr ")"
  ident
  expr infix-op expr
  prefix-op expr
  expr "[" expr "]"
  statement ";" expr
  "if" expr "then" expr "else" expr

infix-op ::=
  "and"
  "or"
  "xor"
  "=="
  "<="
  "<"
  ">="
  ">"
  "+"
  "-"
  "*"
  "/"

prefix-op ::=
  "not"
  "neq0"
  "neg"
  "inv"
  "unpack"
@

== Statement

Statements are a bit odd at the moment, as they are neither
expressions nor the usual statements.

@
statement ::=
  "let" var "=" expr
  "assert" expr
  "for" var "=" int "to" int "do" statements "end"

statements ::=
  (statement (";" statement)*)?
@

== Operator Precedence
  Precedence: "not" > "xor" > "and" > "or"  (but use parentheses anyway).
  if-then-else has to be parenthesised unless it's at the very top.

  Precedence for numeric operators is standard:  {neg,inv} > {*,/} > {+,- }.
  Things like "neg inv 5" are illegal: use parentheses.

  The code is based on the tutorial at
  https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html

  See also https://markkarpov.com/megaparsec/megaparsec.html
-}


module TinyLang.Field.UParser where

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer   as L
import           Data.Void
import qualified Data.Set                     as S
import           GHC.Unicode ( isLower
                             , isDigit)

type Parser = Parsec Void String

type Identifier = String

data Const
  = CBool  Bool
  | CInt   Integer
  | CVec   [Bool]

data Var = Var Identifier

data Expr
  = EConst     Const
  | EVar       Var
  | EAppBinOp  UnOp      Expr
  | EAppUnOp   BinOp     Expr
  | EStatement Statement Expr
  | EIf        Expr      Expr Expr

data BinOp
  = Or
  | And
  | Xor
  | FEq
  | FLe
  | FGe
  | FGt
  | Add
  | Sub
  | Mul
  | Div
  | BAt

data UnOp
  = Not
  | Neq0
  | Neg
  | Inv
  | Unp
  
data Statement
  = ELet    Var Expr
  | EAssert Expr

-- Lexer
sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc


-- Identifier Character
isIdentifierChar :: Char -> Bool
isIdentifierChar c =
  or $ map (\f -> f c) [isLower , isDigit, (=='_'), (=='\'')]


identifierChar :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
identifierChar = satisfy isIdentifierChar <?> "identifier character [a-z0-9_']"
{-# INLINE identifierChar #-}

pKeyword :: String -> Parser String
pKeyword keyword = lexeme (string keyword <* notFollowedBy identifierChar)

keywords :: S.Set String
keywords =
  S.fromList 
    [ "T", "F"
    , "not", "and", "or", "xor"
    , "neq0", "neg", "inv"
    , "let"
    , "if", "then", "else"
    , "for", "do", "end"
    , "unpack"
    ]

pIdentifier :: Parser String
pIdentifier = do
  prefix     <- option "" (string "?" <|> string "#")
  identifier <- (:) <$> lowerChar <*> many identifierChar
  pure $ prefix ++ identifier

