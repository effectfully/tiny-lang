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
    ( parseExpr
    ) where

import           TinyLang.Field.Core
import           TinyLang.Field.Printer
import           TinyLang.Field.ParsableField
import           TinyLang.Field.ParserUtils
import           TinyLang.Prelude               hiding (many, try)
import           TinyLang.Var

import           Control.Monad.Combinators.Expr as E
import qualified Data.Map                       as M
import           Text.Megaparsec
import           Text.Megaparsec.Char

-- | Look up a variable name. If we've already seen it, return the corresponding Var;
-- otherwise, increase the Unique counter and use it to construct a new Var.
makeVar :: (MonadState IdentifierState m) => String -> m Var
makeVar name = do
    (ss, counter) <- get
    case M.lookup name ss of
        Just v -> pure v
        Nothing -> do
            let v = Var (Unique counter) name
                counter' = counter + 1
            put (M.insert name v ss, counter')
            pure v

-- | The main entry point: parse a string and return Either an error message or an Expr.
parseExpr :: ParsableField f => String -> Either String (SomeUniExpr f)
parseExpr s = first errorBundlePretty . fst $ runState (runParserT top "" s) emptyIdentifierState

-- Parse the whole of an input stream
top :: ParsableField f => Parser (SomeUniExpr f)
top = between ws eof exprSome

-- mapExpr :: (forall a. Expr f a -> Expr f a) -> SomeUniExpr f -> SomeUniExpr f
-- mapExpr f (SomeUniExpr uni e) = SomeUniExpr uni $ f e

exprSome :: ParsableField f => Parser (SomeUniExpr f)
exprSome = asum
    [ ifExprSome
    , letExprSome
    , (\(Some uniVar@(UniVar uni _)) -> SomeUniExpr uni $ EVar uniVar) <$> varSome
    , try $ SomeUniExpr Bool <$> expr_OnlyB
    , SomeUniExpr Field <$> expr_OnlyF
    ]
-- ^ Putting FieldExpr first causes trouble with non-parenthesised "1==2", for example.
-- I'm not sure why: it seems to see the 1 and then starts parsing a field expression,
-- but it should backtrack when it fails.  Maybe makeExprParser doesn't backtrack enough?

exprPoly :: forall f a. (ParsableField f, KnownUni f a) => Parser (Expr f a)
exprPoly = asum
    [ ifExprPoly
    , letExprPoly
    , EVar <$> varPoly
    , case knownUni @f @a of
        Bool  -> expr_OnlyB
        Field -> expr_OnlyF
    ]

-- Keywords
keywords :: [String]
keywords = ["T", "F", "not", "and", "or", "xor", "let", "if", "then", "else", "neq0", "neg", "inv"]

-- Parse a keyword, checking that it's not a prefix of something else
keyword :: String -> Parser ()
keyword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)


-- Most of the remaining parsers have a B or F suffix depending on
-- whether they're returning something of type Bool or type Field.

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
valExpr_F :: ParsableField f => Parser (Expr f (AField f))
valExpr_F = EVal . UniVal Field <$> parseFieldElement

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

expr1_B :: ParsableField f => Parser (Expr f Bool)
expr1_B = parens exprPoly <|> valExpr_B <|> neq0Expr <|> eqExpr
-- Let's put parens at the start because we can commit to that if we
-- see "(" and don't have to do any backtracking.

expr1_F :: ParsableField f => Parser (Expr f (AField f))
expr1_F = try valExpr_F <|> parens exprPoly
-- Missing out 'try' before valExpr_F causes a failure with eg (5 % 1) when the field is Rational.
-- We can't put parens at the start because (-5) % 2 is valid syntax for Rationals and we have to try that first.
-- We have to be careful with this because concrete syntax for finite fields might be complicated

-- Special cases for eq and neq0 because the return type isn't the
-- same as the argument type(s).
neq0Expr :: ParsableField f => Parser (Expr f Bool)
neq0Expr = EAppUnOp Neq0 <$ keyword "neq0" <*> exprPoly

eqExpr :: ParsableField f => Parser (Expr f Bool)
eqExpr = EAppBinOp FEq <$> exprPoly <* symbol "==" <*> exprPoly


-- Operations for ordering comparisons of "integer" field elements
-- GADTs stop us using makeExprParsr here: it expects the input and output type to be the same.

-- TODO: try to reduce the number of 'try's. Some parses take a long
-- time and a lot of memory, possibly because of too much
-- backtracking. For example, if we have 'e1 > e2' then I think we
-- have to re-parse e1 each time we fail to match > with one of the
-- other operators.
comparisonExpr :: ParsableField f => Parser (Expr f Bool)
comparisonExpr =
    try (EAppBinOp FLt <$> exprPoly <*> (symbol "<"  *> exprPoly))
            <|> try (EAppBinOp FLe <$> exprPoly <*> (symbol "<=" *> exprPoly))
            <|> try (EAppBinOp FGe <$> exprPoly <*> (symbol ">=" *> exprPoly))
            <|> EAppBinOp FGt <$> exprPoly <*> (symbol ">"  *> exprPoly)

expr_OnlyB :: ParsableField f => Parser (Expr f Bool)
expr_OnlyB = try eqExpr <|> try operExpr_B <|> comparisonExpr

expr_OnlyF :: ParsableField f => Parser (Expr f (AField f))
expr_OnlyF = operExpr_F

-- convert :: forall f a. (KnownUni f a, Show f) => SomeUniExpr f -> Expr f a
-- convert (SomeUniExpr uni e) = withGeqUni uni (knownUni @f @a) e err where
--     err = error $ "type error while parsing: " ++ exprToString NoIDs e

-- exprSome :: ParsableField f => Parser (SomeUniExpr f)
-- exprSome = ifExpr <|> letExprSome

-- -- expr: full expressions
-- expr_B :: ParsableField f => Parser (Expr f Bool)
-- expr_B = exprPoly <|> expr_OnlyB
-- -- Putting if/let at the end leads to some very slow/large parses.
--
-- expr_F :: ParsableField f => Parser (Expr f (AField f))
-- expr_F = exprPoly <|> expr_OnlyF

-- operExpr: expressions involving unary and binary operators.
-- We have to deal with eq and neq0 separately, and also the order
-- comaprisons.

-- Boolean epxressions
operExpr_B :: ParsableField f => Parser (Expr f Bool)
operExpr_B = makeExprParser expr1_B operators_B

operators_B :: [[E.Operator Parser (Expr f Bool)]]
operators_B = -- The order here determines operator precedence.
  [ [Prefix (EAppUnOp  Not <$ keyword "not")]
  , [InfixL (EAppBinOp Xor <$ keyword "xor")]
  , [InfixL (EAppBinOp And <$ keyword "and")]
  , [InfixL (EAppBinOp Or  <$ keyword "or")]
  ]

-- Numeric expressions
operExpr_F :: ParsableField f => Parser (Expr f (AField f))
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

-- -- 'if' with boolean branches
-- ifExpr_B :: ParsableField f => Parser (Expr f Bool)
-- ifExpr_B = EIf
--     <$> (keyword "if" *> expr_B)
--     <*> (keyword "then" *> expr_B)
--     <*> (keyword "else" *> expr_B)

-- -- 'if' with numeric branches
-- ifExpr_F :: ParsableField f => Parser (Expr f (AField f))
-- ifExpr_F = EIf
--     <$> (keyword "if" *> expr_B)
--     <*> (keyword "then" *> expr_F)
--     <*> (keyword "else" *> expr_F)

bind :: UniVar f a -> Expr f a -> SomeUniExpr f -> SomeUniExpr f
bind uniVar body (SomeUniExpr uni e) =
    SomeUniExpr uni $ ELet uniVar body e

-- mapExpr :: (forall a. Expr f a -> Expr f a) -> SomeUniExpr f -> SomeUniExpr f
-- mapExpr f (SomeUniExpr uni expr) = SomeUniExpr uni $ f expr

ifExprSome :: forall f. ParsableField f => Parser (SomeUniExpr f)
ifExprSome =
    (\(cond :: Expr f Bool) (SomeUniExpr uni1 br1) (SomeUniExpr uni2 br2) ->
                SomeUniExpr uni1 . withGeqUni uni1 uni2 (EIf cond br1 br2) $ error "type error")
        <$> (keyword "if" *> exprPoly)
        <*> (keyword "then" *> exprSome)
        <*> (keyword "else" *> exprSome)

ifExprPoly :: (ParsableField f, KnownUni f a) => Parser (Expr f a)
ifExprPoly =
    EIf
        <$> (keyword "if" *> exprPoly)
        <*> (keyword "then" *> exprPoly)
        <*> (keyword "else" *> exprPoly)

letExprSome :: ParsableField f => Parser (SomeUniExpr f)
letExprSome = do
    Some (var@(UniVar varUni _)) <- keyword "let" *> varSome
    withKnownUni varUni $
        (\def (SomeUniExpr bodyUni body) -> SomeUniExpr bodyUni $ ELet var def body)
            <$> (symbol "=" *> exprPoly)
            <*> (symbol ";" *> exprSome)

letExprPoly :: forall f a. (ParsableField f, KnownUni f a) => Parser (Expr f a)
letExprPoly = do
    Some (var@(UniVar varUni _)) <- keyword "let" *> varSome
    withKnownUni varUni $
        ELet var
            <$> (symbol "=" *> exprPoly)
            <*> (symbol ";" *> exprPoly)



-- letExprSome :: forall f. ParsableField f => Parser (SomeUniExpr f)
-- letExprSome =
--     (\(Some v@(UniVar uni1 _)) (SomeUniExpr uni2 def) (SomeUniExpr uni body) ->
--                 SomeUniExpr uni . withGeqUni uni1 uni2 (ELet v def body) $ error "type error")
--         <$> (keyword "let" *> varSome)
--         <*> (symbol  "="   *> exprSome @f)
--         <*> (symbol  ";"   *> exprSome)

-- letExprPoly :: forall f a. (ParsableField f, KnownUni f a) => Parser (Expr f a)
-- letExprPoly =
--     (\(Some v@(UniVar uni1 _)) (SomeUniExpr uni2 def) (body :: Expr f a) ->
--                 withGeqUni uni1 uni2 (ELet v def body) $ error "type error")
--         <$> (keyword "let" *> varSome)
--         <*> (symbol  "="   *> exprSome @f)
--         <*> (symbol  ";"   *> exprPoly)




-- -- 'let' with boolean type
-- letExpr_B :: ParsableField f => Parser (Expr f Bool)
-- letExpr_B =
--     try (ELet
--         <$> (keyword "let" *> var_B)
--         <*> (symbol  "="   *> expr_B)
--         <*> (symbol  ";"   *> expr_B))
--     <|> (ELet
--         <$> (keyword "let" *> var_F)
--         <*> (symbol  "="   *> expr_F)
--         <*> (symbol  ";"   *> expr_B))

-- -- 'let' with numeric type
-- letExpr_F :: ParsableField f => Parser (Expr f (AField f))
-- letExpr_F =
--     try (ELet
--         <$> (keyword "let" *> var_B)
--         <*> (symbol  "="   *> expr_B)
--         <*> (symbol  ";"   *> expr_F))
--      <|> (ELet
--         <$> (keyword "let" *> var_F)
--         <*> (symbol  "="   *> expr_F)
--         <*> (symbol  ";"   *> expr_F))
