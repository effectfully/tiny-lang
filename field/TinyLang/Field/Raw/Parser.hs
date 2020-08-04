{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
= Raw Parser

A simple parser for language involving booleans, field elements, and
vectors.

Given that the language is simple enough we are mixing lexical
analysis and grammar a bit.  To avoid any ambiguities between lexical
analysis and grammar we will explicitly mark all lexical tokens.

Please note that expressions are the designated start production for
the parser.

As convention we enclose /tokens/ in parentheses @""@.

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
    "field"
    "bool"
    "vector"
    "ext"
@

== Types/Universes

Currently we only support 3 types/universes:

* booleans,
* fields, and
* vectors.

We use them to annotate expressions with their desired type.

@
uni ::=
    "bool"
    "field"
    "vector"
@

== Identifiers

To avoid a syntactic clash with /bool-literals/, identifiers start with a
lower-case letter and we check that the identifier is not a keyword.

@
ident ::= [a-z] ([a-z0-9_'])*
@

== Variable Declarations

We follow the ML-family syntax for variable declarations, where the identifier
(@ident@) is followed by a colon (@:@) and the type of the variable (@uni@).

@
var-decl ::=
    ident ":" uni
@


== Constants

At the moment constants consist of literals only.

@
const ::=
    bool-literal
    int-literal
    vec-literal
@

An @int-literal@ is converted to a field element via 'fromInteger'.

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
    expr ":" uni

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

@
statement ::=
    "let" var-decl "=" expr
    "assert" expr
    "for" var "=" int-literal "to" int-literal "do" statements "end"

statements ::=
    (statement ";")*
@

== Program

A program is a list of external declarations followed by statements.

@
program ::=
    ext-decls statements

ext-decls ::=
    (ext-decl ";")*

ext-decl ::=
    "ext" var-decl
@

== Operator Precedence

We use the following operator precedence:

- unary operators,
- vector element access ([]),
- @*, /@,
- @+, -@,
- arithmetic comparison operators, and
- boolean operators.

The code is based on the tutorial at
https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html

See also https://markkarpov.com/megaparsec/megaparsec.html
-}


module TinyLang.Field.Raw.Parser
    ( pTop
    ) where

import           TinyLang.Prelude               hiding (many, option, try)

import           Data.Field
import           TinyLang.Field.Existential
import           TinyLang.Field.Raw.Core
import           TinyLang.Field.Uni
import           TinyLang.ParseUtils

import qualified Control.Monad.Combinators.Expr as Comb
import           Data.Set                       (fromList, member)
import qualified Data.Vector                    as Vector
import           Text.Megaparsec
import           Text.Megaparsec.Char

type ParserT = ParsecT Void String

{-| == IsString instances
-}

instance TextField f => IsString (RawProgram f) where
    fromString = either error id . parseString (pTop @f) ""

{-| == Lexer
-}

-- Identifier Character
isIdentifierChar :: Char -> Bool
isIdentifierChar c =
    any (\f -> f c) [isLower , isDigit, (=='_'), (=='\'')]

-- Parser label for identifier charactersr
identifierCharLabel :: String
identifierCharLabel = "identifier character [a-z0-9_']"

identifierChar :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
identifierChar = satisfy isIdentifierChar <?> identifierCharLabel

keyword :: String -> ParserT m ()
keyword kwd = lexeme (string kwd *> notFollowedBy identifierChar)

keywords :: Set String
keywords =
    fromList
    [ "T", "F"
    , "not", "and", "or", "xor"
    , "neq0", "neg", "inv"
    , "unpack"
    , "let"
    , "assert"
    , "for", "do", "end"
    , "if", "then", "else"
    , "bool", "field", "vector"
    , "ext"
    ]

isKeyword :: String -> Bool
isKeyword = (`member` keywords)


pUni :: ParserT m (SomeUni f)
pUni = choice
    [ Some Bool   <$ keyword "bool"
    , Some Field  <$ keyword "field"
    , Some Vector <$ keyword "vector"
    ]


-- TODO:  Consider merging Identifier with Variable
pIdentifier :: ParserT m Identifier
pIdentifier =
    lexeme $ do
        (:) <$> lowerChar
            <*> takeWhileP (Just identifierCharLabel) isIdentifierChar

-- variable is an identifier that is not a keyword
pVar :: ParserT m Var
pVar = do
    ident <- pIdentifier
    when (isKeyword ident)
         (fail ("keyword " ++ show ident ++ " cannot be an identifier"))
    pure $ Var ident

-- variable declaration
pVarDecl :: ParserT m (Var, SomeUni f)
pVarDecl = (,) <$> pVar <*> (symbol ":" *> pUni)

pBoolLiteral :: ParserT m Bool
pBoolLiteral =
    lexeme $ charToBool <$> (satisfy isTF <?> "T or F")
    where
        isTF x = x == 'T' || x == 'F'
        --
        charToBool 'T' = True
        charToBool 'F' = False
        charToBool _   = error "impossible"

pVecLiteral :: ParserT m (Vector Bool)
pVecLiteral =
    lexeme $
        Vector.fromList <$>
        between
            (symbol "{")
            (symbol "}")
            (pBoolLiteral `sepBy` (symbol ","))

pIntLiteral :: ParserT m Integer
pIntLiteral = signedDecimal



{-| == Parser
-}

unary :: ParserT m a -> UnOp -> Comb.Operator (ParserT m) (RawExpr f)
unary pName op = Comb.Prefix (EAppUnOp op <$ pName)

binary :: ParserT m a -> BinOp -> Comb.Operator (ParserT m) (RawExpr f)
binary pName op = Comb.InfixL (EAppBinOp op <$ pName)

pIndex :: Field f => ParserT m (RawExpr f)
pIndex = brackets pExpr

operatorTable :: Field f => [[Comb.Operator (ParserT m) (RawExpr f)]]
operatorTable =
    [ [ unary  (keyword "not")    $ Not
      , unary  (keyword "neg")    $ Neg
      , unary  (keyword "inv")    $ Inv
      , unary  (keyword "neq0")   $ Neq0
      , unary  (keyword "unpack") $ Unp
      ]
      -- expr [ expr ]
    , [ Comb.Postfix (EAppBinOp BAt <$> pIndex)
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
      -- : uni
    , [ Comb.Postfix (ETypeAnn <$> pAnn)
      ]
    ]

vBool :: ParserT m (SomeUniConst f)
vBool  = Some . UniConst Bool <$> pBoolLiteral

vVec :: ParserT m (SomeUniConst f)
vVec   = Some . UniConst Vector <$> pVecLiteral

vField :: Field f => ParserT m (SomeUniConst f)
vField = Some . UniConst Field . fromInteger <$> signedDecimal

pConst :: Field f => ParserT m (SomeUniConst f)
pConst = choice
    [ vBool
    , vVec
    , vField
    ]


pAnn :: ParserT m (SomeUni f)
pAnn = symbol ":" *> pUni

pTerm :: Field f => ParserT m (RawExpr f)
pTerm =
    choice
    -- This can backtrack for parentheses
    [ try (EConst   <$> pConst)
    -- This can backtrack for keywords
    , try (EVar     <$> pVar)
    , EIf           <$> (keyword "if"   *> pExpr)
                    <*> (keyword "then" *> pExpr)
                    <*> (keyword "else" *> pExpr)
    , parens pExpr
    ]

pExpr :: Field f => ParserT m (RawExpr f)
pExpr = Comb.makeExprParser pTerm operatorTable

pStatement :: Field f => ParserT m (RawStatement f)
pStatement =
    choice
    -- This can backtrack for expr starting with a "("
    [ try (parens pStatement)
    , ELet    <$> (keyword "let"    *> pVarDecl)
              <*> (symbol  "="      *> pExpr)
    , EAssert <$> (keyword "assert" *> pExpr)
    , EFor    <$> (keyword "for"    *> pVar)
              <*> (symbol  "="      *> pIntLiteral)
              <*> (keyword "to"     *> pIntLiteral)
              <*> (keyword "do"     *> pStatements)
              <*   keyword "end"
    ]

pExtDecl :: ParserT m (Var, SomeUni f)
pExtDecl = keyword "ext" *> pVarDecl

pExtDecls :: ParserT m [(Var, SomeUni f)]
pExtDecls = many (pExtDecl <* symbol ";")

pStatements :: Field f => ParserT m (RawStatements f)
pStatements =
    choice
    -- This can backtrack for statement starting with a "("
    [ try (parens pStatements)
    , Statements <$> many (pStatement <* symbol ";")
    ]

pProgram :: Field f => ParserT m (RawProgram f)
pProgram = Program <$> pExtDecls <*> pStatements

pTop :: Field f => ParserT m (RawProgram f)
pTop = top pProgram
