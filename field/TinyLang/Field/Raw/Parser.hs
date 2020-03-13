{-|
= Raw Parser

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
Precedence: "not" > "xor" > "and" > "or" (but use parentheses anyway).
if-then-else has to be parenthesised unless it's at the very top.

Precedence for numeric operators is standard:  {neg,inv} > {*,/} > {+,- }.
Things like "neg inv 5" are illegal: use parentheses.

The code is based on the tutorial at
https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html

See also https://markkarpov.com/megaparsec/megaparsec.html
-}


module TinyLang.Field.Raw.Parser where

import           TinyLang.Prelude hiding ( Const
                                         , option
                                         , many
                                         )
-- import           TinyLang.Field.Core
import           Data.Set ( fromList
                          , member
                          )
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L
-- import           Text.Megaparsec.Debug
import qualified Control.Monad.Combinators.Expr as Comb

type Parser = Parsec Void String

type Identifier = String

data Const
    = CBool  Bool
    | CInt   Integer
    | CVec   [Bool]
    deriving (Show)

data Var = Var Identifier
    deriving (Show)

data Expr f
    = EConst     Const
    | EVar       Var
    | EAppBinOp  BinOp         (Expr f) (Expr f)
    | EAppUnOp   UnOp          (Expr f)
    | EStatement (Statement f) (Expr f)
    | EIf        (Expr f)      (Expr f) (Expr f)
    deriving (Show)

data BinOp
    = Or
    | And
    | Xor
    | FEq
    | FLe
    | FLt
    | FGe
    | FGt
    | Add
    | Sub
    | Mul
    | Div
    | BAt
    deriving (Show)

data UnOp
    = Not
    | Neq0
    | Neg
    | Inv
    | Unp
    deriving (Show)


data Statement f
    = ELet    Var      (Expr f)
    | EAssert (Expr f)
    | EFor    Var      (Expr f) (Expr f) [Statement f]
    deriving (Show)

-- Lexer
sc :: Parser ()
sc = L.space space1
             (L.skipLineComment "#")
             empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc


-- Identifier Character
isIdentifierChar :: Char -> Bool
isIdentifierChar c =
    any (\f -> f c) [isLower , isDigit, (=='_'), (=='\'')]

-- Parser label for identifier charactersr
identifierCharLabel :: String
identifierCharLabel = "identifier character [a-z0-9_']"

identifierChar :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
identifierChar = satisfy isIdentifierChar <?> identifierCharLabel

keyword :: String -> Parser ()
keyword kwd = lexeme (string kwd *> notFollowedBy identifierChar)

keywords :: Set String
keywords =
    fromList
    [ "T", "F"
    , "not", "and", "or", "xor"
    , "neq0", "neg", "inv"
    , "let"
    , "if", "then", "else"
    , "for", "do", "end"
    , "unpack"
    ]

isKeyword :: String -> Bool
isKeyword = (`member` keywords)

pIdentifier :: Parser Identifier
pIdentifier =
    lexeme $ do
        prefix     <- option "" (string "?" <|> string "#")
        identifier <- (:) <$> lowerChar
                          <*> takeWhileP (Just identifierCharLabel)
                              isIdentifierChar
        pure $ prefix ++ identifier

pBoolLiteral :: Parser Bool
pBoolLiteral =
    lexeme $ charToBool <$> (satisfy isTF <?> "T or F")
    where
        isTF x = x == 'T' || x == 'F'
        --
        charToBool 'T' = True
        charToBool 'F' = False
        charToBool _   = error "impossible"

pIntLiteral :: Parser Integer
pIntLiteral =
    lexeme $ read <$> takeWhile1P (Just "digit") isDigit

pVecLiteral :: Parser [Bool]
pVecLiteral =
    lexeme $
        between
            (symbol "{")
            (symbol "}")
            (pBoolLiteral `sepBy` (symbol ","))

pConst :: Parser Const
pConst =
    choice
        [ CBool <$> pBoolLiteral
        , CInt  <$> pIntLiteral
        , CVec  <$> pVecLiteral
        ]

-- variable is an identifier that is not a keyword
pVar :: Parser Var
pVar = do
    ident <- pIdentifier
    when (isKeyword ident)
         (fail ("keyword " ++ show ident ++ " cannot be an identifier"))
    pure $ Var ident

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

unary :: forall a f.  Parser a -> UnOp -> Comb.Operator Parser (Expr f)
unary pName op = Comb.Prefix (EAppUnOp op <$ pName)

binary :: forall a f.  Parser a -> BinOp -> Comb.Operator Parser (Expr f)
binary pName op = Comb.InfixL (EAppBinOp op <$ pName)

pIndex :: forall f . Parser (Expr f)
pIndex = brackets pExpr

operatorTable :: forall f. [[Comb.Operator Parser (Expr f)]]
operatorTable =
    [ [ unary  (keyword "not")    $ Not
      , unary  (keyword "neg")    $ Neg
      , unary  (keyword "inv")    $ Inv
      , unary  (keyword "neq0")   $ Neq0
      , unary  (keyword "unpack") $ Unp
      ]
      -- expr [ expr ]
    , [ Comb.Postfix (flip (EAppBinOp BAt) <$> pIndex)
      ]
    , [ binary (symbol  "*")      $ Mul
      , binary (symbol  "/")      $ Div
      ]
    , [ binary (symbol  "+")      $ Add
      , binary (symbol  "-")      $ Sub
      ]
    , [ binary (symbol  "==")     $ FEq
      , binary (symbol  "<=")     $ FLe
      , binary (symbol  "<")      $ FLt
      , binary (symbol  ">=")     $ FGe
      , binary (symbol  ">")      $ FGt
      ]
    , [ binary (keyword "xor")    $ Xor
      , binary (keyword "and")    $ And
      , binary (keyword "or")     $ Or
      ]
    ]

pTerm :: forall f. Parser (Expr f)
pTerm =
    choice
    [ EConst        <$> pConst
    , parens pExpr
    , EStatement    <$> pStatement <* symbol ";"  <*> pExpr
    , EVar          <$> pVar
    ]

pStatements :: forall f. Parser [Statement f]
pStatements = many (pStatement <* symbol ";")

pStatement :: forall f. Parser (Statement f)
pStatement =
    choice
    [ keyword "let"    $> ELet    <*> pVar <* symbol "=" <*> pExpr
    , keyword "assert" $> EAssert <*> pExpr
    , keyword "for"    $> EFor    <*> pVar <* symbol "="
                                  <*> pExpr <* keyword "to"
                                  <*> pExpr <* keyword "do"
                                  <*> pStatements
                                  <* keyword "end"
    ]

pExpr :: forall f. Parser (Expr f)
pExpr = Comb.makeExprParser pTerm operatorTable

pTopLevel :: forall f. Parser (Expr f)
pTopLevel = between sc eof pExpr

parseString :: String -> String -> String
parseString fileName str =
    either
      errorBundlePretty
      show
      $ runParser pTopLevel fileName str

p :: String -> IO ()
p s = putStrLn $ parseString "" s
