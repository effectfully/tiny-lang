{-| A parser for a tiny language involving booleans and field elements.
  The concrete syntax is as follows:

  val ::= T | F
  fvar ::= [a-z][a-z0-9_]*
  bvar ::=  '?'[a-z][a-z0-9_]*

  Note that boolean variable names must begin with '?' so that
  the parser knows what the type is.  We'd need environments
  or type annotations or something to avoid this.

  expr ::= val
           fvar
           bvar
           'not' expr
           'neq0' expr
           'neg' expr
           'inv' expr
           expr 'and' expr
           expr 'or'  expr
           expr 'xor' expr
           expr == expr
           expr < expr
           expr <= expr
           expr >= expr
           expr > expr
           expr + expr
           expr - expr
           expr * expr
           expr / expr
           'if' expr 'then' expr 'else' expr
           'let' var = expr; expr
           'assert' expr == expr; expr
           (expr)

  Things like 'and' denote keywords.

  Precedence: 'not' > 'xor' > 'and' > 'or'  (but use parentheses anyway).
  if-then-else has to be parenthesised unless it's at the very top.

  Precedence for numeric operators is standard:  {neg,inv} > {*,/} > {+,- }.
  Things like "neg inv 5" are illegal: use parentheses.

  The code is based on the tutorial at
  https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html

  See also https://markkarpov.com/megaparsec/megaparsec.html
-}

-- FIXME: do we want to allow == on booleans?  Eg, T==F or (1==2)==(3==4)

module TinyLang.Field.Parser
    ( parseBy
    , parseExprScope
    , parseEConstrScope
    , parseExpr
    , parseEConstr
    ) where

import           TinyLang.Prelude               hiding (many, try)

import           TinyLang.Field.Core
import           TinyLang.ParseUtils

import           Control.Monad.Combinators.Expr as E
import qualified Data.Map                       as M
import           Text.Megaparsec
import           Text.Megaparsec.Char

-- | Look up a variable name. If we've already seen it, return the corresponding Var;
-- otherwise, increase the Unique counter and use it to construct a new Var.
makeVar :: (MonadSupply m, MonadState Scope m) => String -> m Var
makeVar name = do
    vars <- get
    case M.lookup name vars of
        Just var -> pure var
        Nothing  -> do
            var <- freshVar name
            put $ M.insert name var vars
            pure var

-- | Parse a @String@ and return @Either@ an error message or an @Expr@ of some type.
-- Regardless of whether the result is an error or not, also returns the latest 'Scope'.
parseExprScope
    :: forall f m. (MonadSupply m, TextField f)
    => String -> m (Either String (SomeUniExpr f), Scope)
parseExprScope = parseBy expr

-- | Parse a @String@ and return @Either@ an error message or an @EConstr@.
-- Regardless of whether the result is an error or not, also returns the latest 'Scope'.
parseEConstrScope
    :: forall f m. (MonadSupply m, TextField f)
    => String -> m (Either String (EConstr f), Scope)
parseEConstrScope = parseBy econstr

-- | Parse a @String@ and return @Either@ an error message or an @Expr@ of some type.
parseExpr
    :: forall f m. (MonadSupply m, TextField f)
    => String -> m (Either String (SomeUniExpr f))
parseExpr = fmap fst . parseExprScope

-- | Parse a @String@ and return @Either@ an error message or an @EConstr@.
parseEConstr
    :: forall f m. (MonadSupply m, TextField f)
    => String -> m (Either String (EConstr f))
parseEConstr = fmap fst . parseEConstrScope

parseBy :: MonadSupply m => Parser a -> String -> m (Either String a, Scope)
parseBy parser str =
    liftSupply $ first (first errorBundlePretty) <$>
        runStateT (runParserT (top parser) "" str) emptyScope

-- Parse the whole of an input stream
top :: Parser a -> Parser a
top = between ws eof

expr :: TextField f => Parser (SomeUniExpr f)
expr = try (SomeUniExpr Bool <$> exprPoly) <|> (SomeUniExpr Field <$> exprPoly)
-- ^ Putting FieldExpr first causes trouble with non-parenthesised "1==2", for example.
-- I'm not sure why: it seems to see the 1 and then starts parsing a field expression,
-- but it should backtrack when it fails.  Maybe makeExprParser doesn't backtrack enough?

-- Keywords
keywords :: [String]
keywords = ["T", "F", "not", "and", "or", "xor", "let", "if", "then", "else", "neq0", "neg", "inv"]

-- Parse a keyword, checking that it's not a prefix of something else
keyword :: String -> Parser ()
keyword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

-- For type disambiguation purposes variables of type Field have
-- normal ids and ones of type Bool have ids beginning with '?'
identifier_F :: Parser String
identifier_F = (lexeme . try) (p >>= check)
    where
      p       = (:) <$> lowerChar <*> many (lowerChar <|> digitChar <|> char '_')
      check x = if x `elem` keywords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

identifier_B :: Parser String
identifier_B = (lexeme . try) (p >>= check)
    where
      p       = (:) <$> char '?' <*> many (lowerChar <|> digitChar <|> char '_')
      check x = if x `elem` keywords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

-- Constants T and F
trueVal :: Parser (Expr f Bool)
trueVal =  EVal (UniVal Bool True) <$ keyword "T"

falseVal :: Parser (Expr f Bool)
falseVal = EVal (UniVal Bool False) <$ keyword "F"

valExpr_B :: Parser (Expr f Bool)
valExpr_B = trueVal <|> falseVal

-- Literal constants from the field
valExpr_F :: TextField f => Parser (Expr f (AField f))
valExpr_F = EVal . UniVal Field <$> parseField

-- Variables
varExpr_F :: Parser (Expr f (AField f))
varExpr_F = EVar <$> var_F

varExpr_B :: Parser (Expr f Bool)
varExpr_B = EVar <$> var_B

var_F :: Parser (UniVar f (AField f))
var_F = UniVar Field <$> (identifier_F >>= makeVar)

var_B :: Parser (UniVar f Bool)
var_B = UniVar Bool <$> (identifier_B >>= makeVar)

varSome :: Parser (SomeUniVar f)
varSome = Some <$> var_B <|> Some <$> var_F

varPoly :: forall f a. KnownUni f a => Parser (UniVar f a)
varPoly = case knownUni @f @a of
    Bool  -> var_B
    Field -> var_F

{- Use the Expr combinators from Control.Monad.Combinators.Expr to parse
   epressions involving prefix and infix operators.  This makes it a
   lot easier to get parsing of expressions right. It deals with
   precedence automatically and avoids problems with left recursion
   that may lead to non-terminating parses if you're not careful about
   binary infix expressions.
-}

-- expr1: things that can appear inside operExpr. This does not
-- include operExpr itself, because that would cause infinite recursion.
-- Note that an operExpr doesn't have to contain an operator: it
-- can just be a single expr1.
-- If an ifExpr appears inside an operExpr it has to be parenthesised.

expr1_B :: TextField f => Parser (Expr f Bool)
expr1_B =  parens exprPoly <|> valExpr_B <|> varExpr_B <|> neq0Expr <|> eqExpr
-- Let's put parens at the start because we can commit to that if we
-- see "(" and don't have to do any backtracking.

expr1_F :: TextField f => Parser (Expr f (AField f))
expr1_F =  try valExpr_F <|> parens exprPoly <|> varExpr_F
-- Missing out 'try' before valExpr_F causes a failure with eg (5 % 1) when the field is Rational.
-- We can't put parens at the start because (-5) % 2 is valid syntax for Rationals and we have to try that first.
-- We have to be careful with this because concrete syntax for finite fields might be complicated

-- Special cases for eq and neq0 because the return type isn't the
-- same as the argument type(s).
neq0Expr :: TextField f => Parser (Expr f Bool)
neq0Expr = EAppUnOp Neq0 <$ keyword "neq0" <*> exprPoly

eqExpr :: TextField f => Parser (Expr f Bool)
eqExpr = EAppBinOp FEq <$> exprPoly <* symbol "==" <*> exprPoly

-- Operations for ordering comparisons of "integer" field elements
-- GADTs stop us using makeExprParsr here: it expects the input and output type to be the same.

-- TODO: try to reduce the number of 'try's. Some parses take a long
-- time and a lot of memory, possibly because of too much
-- backtracking. For example, if we have 'e1 > e2' then I think we
-- have to re-parse e1 each time we fail to match > with one of the
-- other operators.
comparisonExpr :: TextField f => Parser (Expr f Bool)
comparisonExpr = asum
    [ try $ EAppBinOp FLt <$> exprPoly <*> (symbol "<"  *> exprPoly)
    , try $ EAppBinOp FLe <$> exprPoly <*> (symbol "<=" *> exprPoly)
    , try $ EAppBinOp FGe <$> exprPoly <*> (symbol ">=" *> exprPoly)
    , EAppBinOp FGt <$> exprPoly <*> (symbol ">"  *> exprPoly)
    ]

exprPoly :: forall f a. (TextField f, KnownUni f a) => Parser (Expr f a)
exprPoly = asum
    [ ifExprPoly
    , letExprPoly
    , eqConstrPoly
    , EVar <$> varPoly
    , case knownUni @f @a of
        Bool  -> asum
            [ try eqExpr
            , try operExpr_B
            , comparisonExpr
            ]
        Field -> operExpr_F
    ]

-- operExpr: expressions involving unary and binary operators.
-- We have to deal with eq and neq0 separately, and also the order
-- comaprisons.

-- Boolean epxressions
operExpr_B :: TextField f => Parser (Expr f Bool)
operExpr_B = makeExprParser expr1_B operators_B

operators_B :: [[E.Operator Parser (Expr f Bool)]]
operators_B = -- The order here determines operator precedence.
    [ [Prefix (EAppUnOp  Not <$ keyword "not")]
    , [InfixL (EAppBinOp Xor <$ keyword "xor")]
    , [InfixL (EAppBinOp And <$ keyword "and")]
    , [InfixL (EAppBinOp Or  <$ keyword "or")]
    ]

-- Numeric expressions
operExpr_F :: TextField f => Parser (Expr f (AField f))
operExpr_F = makeExprParser expr1_F operators_F

operators_F :: [[E.Operator Parser (Expr f (AField f))]]
operators_F = -- The order here determines operator precedence.
    [ [Prefix (EAppUnOp  Neg <$ keyword "neg"), Prefix (EAppUnOp Inv <$ keyword "inv")]
    , [InfixL (EAppBinOp Mul <$ symbol "*"), InfixL (EAppBinOp Div <$ symbol "/")]
    , [InfixL (EAppBinOp Add <$ symbol "+"), InfixL (EAppBinOp Sub <$ symbol "-")]
    ]

-- Can we somehow commit to an if-expression when we see "if expr_B" and then
-- continue with the other cases?  This would reduce the need for backtracking.
-- Probably not, because we don't know the type of the entire expression until
-- we see the first branch.

ifExprPoly :: (TextField f, KnownUni f a) => Parser (Expr f a)
ifExprPoly = EIf
    <$> (keyword "if" *> exprPoly)
    <*> (keyword "then" *> exprPoly)
    <*> (keyword "else" *> exprPoly)

letExprPoly :: (TextField f, KnownUni f a) => Parser (Expr f a)
letExprPoly = do
    Some (var@(UniVar varUni _)) <- keyword "let" *> varSome
    withKnownUni varUni $ ELet var
        <$> (symbol "=" *> exprPoly)
        <*> (symbol ";" *> exprPoly)

econstr :: TextField f => Parser (EConstr f)
econstr =
    EConstrFEq
        <$> (keyword "assert" *> exprPoly)
        <*> (symbol  "=="     *> exprPoly)

eqConstrPoly :: (TextField f, KnownUni f a) => Parser (Expr f a)
eqConstrPoly =
    EConstr
        <$> econstr
        <*> (symbol ";" *> exprPoly)
