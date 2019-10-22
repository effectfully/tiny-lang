{-| A parser for a tiny language involving booleans and field elements.
  The concrete syntax is as follows:

  val ::= T | F
  fvar ::= [a-z][a-z0-9_]*
  bvar ::=  '?'[a-z][a-z0-9_]*

  Note that boolean variable names must begin with '?' so that
  the parser knows what the type is.  We'd need environments
  or type annotations or something to avoid this.

  assertion ::=
      'assert' expr == expr

  statements ::=
      null
      statement; statements

  statement ::=
      'let' var = expr
      assertion
      'for' var = int 'to' int 'do' statements 'end'

  expr ::=
      val
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
      statement; expr
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
    , parseExpr
    ) where

import           TinyLang.Prelude               hiding (many, some, try, option)

import           TinyLang.Field.Core
import           TinyLang.Field.Rename
import           TinyLang.Field.Evaluator
import           TinyLang.ParseUtils

import           Control.Monad.Combinators.Expr as Comb
import qualified Data.IntMap.Strict             as IntMap
import qualified Data.IntSet                    as IntSet
import qualified Data.Map.Strict                as Map
import qualified Data.Vector as Vector
import           Text.Megaparsec
import           Text.Megaparsec.Char

-- | Look up a variable name. If we've already seen it, return the corresponding Var;
-- otherwise, increase the Unique counter and use it to construct a new Var.
makeVar :: String -> Parser Var
makeVar name = do
    vars <- get
    case Map.lookup name vars of
        Just var -> pure var
        Nothing  -> do
            var <- freshVar name
            put $ Map.insert name var vars
            pure var

-- | Parse a @String@ and return @Either@ an error message or an @Expr@ of some type.
-- Regardless of whether the result is an error or not, also returns the latest 'Scope'.
parseExprScope
    :: forall f m. (MonadSupply m, TextField f)
    => String -> m (Either String (SomeUniExpr f), Scope)
parseExprScope str = do
    (errOrSomeUniExpr, totalScope) <- parseBy expr str
    case errOrSomeUniExpr of
        Left  err            -> return (Left err, totalScope)
        Right (SomeOf uni e) -> do
            eRen <- renameExpr e
            let freeIndices = IntMap.keysSet . unEnv $ exprFreeVarSigns eRen
                isFree var = unUnique (_varUniq var) `IntSet.member` freeIndices
                freeScope = Map.filter isFree totalScope
            return $ (Right $ SomeOf uni eRen, freeScope)

-- | Parse a @String@ and return @Either@ an error message or an @Expr@ of some type.
parseExpr
    :: forall f m. (MonadSupply m, TextField f)
    => String -> m (Either String (SomeUniExpr f))
parseExpr = fmap fst . parseExprScope

parseBy :: MonadSupply m => Parser a -> String -> m (Either String a, Scope)
parseBy parser str =
    liftSupply $ first (first errorBundlePretty) <$>
        runStateT (runParserT (top parser) "" str) emptyScope

-- Parse the whole of an input stream
top :: Parser a -> Parser a
top = between ws eof

expr :: TextField f => Parser (SomeUniExpr f)
expr = asum
    [ try $ SomeOf Bool   <$> exprPoly
    , try $ SomeOf Field  <$> exprPoly
    ,       SomeOf Vector <$> exprPoly
    ]
-- Putting FieldExpr first causes trouble with non-parenthesised "1==2", for example.
-- I'm not sure why: it seems to see the 1 and then starts parsing a field expression,
-- but it should backtrack when it fails.  Maybe makeExprParser doesn't backtrack enough?

-- Keywords
keywords :: [String]
keywords =
    [ "T", "F"
    , "not", "and", "or", "xor"
    , "neq0", "neg", "inv"
    , "let"
    , "if", "then", "else"
    , "for", "do", "end"
    , "unpack"
    ]

-- Parse a keyword, checking that it's not a prefix of something else
keyword :: String -> Parser ()
keyword w = lexeme . try $ string w *> notFollowedBy alphaNumChar

nonKeyword :: String -> Parser String
nonKeyword ident
    | ident `elem` keywords = fail $ "keyword " ++ show ident ++ " cannot be an identifier"
    | otherwise             = return ident

makeIdentifier :: Parser String -> Parser String
makeIdentifier p = lexeme . try $ p >>= nonKeyword

-- For type disambiguation purposes variables of type Field have
-- normal ids and ones of type Bool have ids beginning with '?'

varCommon :: Parser String
varCommon = many (lowerChar <|> digitChar <|> oneOf @[] "_\'")

varString_F :: Parser String
varString_F = (:) <$> lowerChar <*> varCommon

varString_B :: Parser String
varString_B = (:) <$> char '?' <*> varCommon

varString_V :: Parser String
varString_V = (:) <$> char '#' <*> varCommon

-- Constants T and F
parseBool :: Parser Bool
parseBool = True <$ keyword "T" <|> False <$ keyword "F"

parseVector :: Parser (Vector Bool)
parseVector = fmap Vector.fromList $ symbol "{" *> parseBool `sepBy` (symbol ",") <* symbol "}"

uniValPoly :: forall f a. (TextField f, KnownUni f a) => Parser (UniVal f a)
uniValPoly =
    UniVal uni <$> case uni of
        Bool   -> parseBool
        -- Literal constants from the field
        Field  -> parseField
        Vector -> parseVector
    where
        uni = knownUni @f @a

var_F :: Parser Var
var_F = makeIdentifier varString_F >>= makeVar

var_B :: Parser Var
var_B = makeIdentifier varString_B >>= makeVar

var_V :: Parser Var
var_V = makeIdentifier varString_V >>= makeVar

uniVar_F :: Parser (UniVar f (AField f))
uniVar_F = UniVar Field <$> var_F

uniVar_B :: Parser (UniVar f Bool)
uniVar_B = UniVar Bool <$> var_B

uniVar_V :: Parser (UniVar f (Vector Bool))
uniVar_V = UniVar Vector <$> var_V

varSome :: Parser (SomeUniVar f)
varSome = asum
    [ Some <$> uniVar_B
    , Some <$> uniVar_F
    , Some <$> uniVar_V
    ]

uniVarPoly :: forall f a. KnownUni f a => Parser (UniVar f a)
uniVarPoly = case knownUni @f @a of
    Bool   -> uniVar_B
    Field  -> uniVar_F
    Vector -> uniVar_V

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

expr1 :: forall f a. (TextField f, KnownUni f a) => Parser (Expr f a)
expr1 = asum
    -- Let's put parens at the start because we can commit to that if we see "(" and
    -- don't have to do any backtracking. Note that syntax for fields must not interfere with
    -- general parsing. E.g. we can't use the standard (and silly) Haskell's syntax for rationals,
    -- because we wouldn't be able to parse, say, @(-5) % 2@. We have to be careful with this
    -- because concrete syntax for finite fields might be complicated
    [ parens exprPoly
    , EVar <$> uniVarPoly
    , EVal <$> uniValPoly
    , case knownUni @f @a of
        Bool   -> neq0Expr <|> eqExpr
        Field  -> empty
        Vector -> unpackExpr
    ]

-- Special cases for eq and neq0 because the return type isn't the
-- same as the argument type(s).
neq0Expr :: TextField f => Parser (Expr f Bool)
neq0Expr = EAppUnOp Neq0 <$ keyword "neq0" <*> exprPoly

eqExpr :: TextField f => Parser (Expr f Bool)
eqExpr = EAppBinOp FEq <$> exprPoly <* symbol "==" <*> exprPoly

unpackExpr :: TextField f => Parser (Expr f (Vector Bool))
unpackExpr = EAppUnOp Unp <$ keyword "unpack" <*> exprPoly

-- Operations for ordering comparisons of "integer" field elements
-- GADTs stop us using makeExprParsr here: it expects the input and output type to be the same.

comparisonExpr :: TextField f => Parser (Expr f Bool)
comparisonExpr = do
    -- Regardless of what operator goes next, we need to parse the left-hand argument first,
    -- so that we don't need to reparse it for each of the options.
    l <- exprPoly
    -- @symbol@ auto-backtracks, so no need to use @try@.
    asum
        [ EAppBinOp FLe l <$> (symbol "<=" *> exprPoly)
        , EAppBinOp FLt l <$> (symbol "<"  *> exprPoly)
        , EAppBinOp FGe l <$> (symbol ">=" *> exprPoly)
        , EAppBinOp FGt l <$> (symbol ">"  *> exprPoly)
        ]

exprPoly :: forall f a. (TextField f, KnownUni f a) => Parser (Expr f a)
exprPoly = asum
    [ ifExprPoly
    , estatementPoly
    , try operExpr
    , case knownUni @f @a of
        Bool   -> asum
            [ try eqExpr
            , try comparisonExpr
            , batExprPoly
            ]
        Field  -> empty
        Vector -> empty
    ]

-- operExpr: expressions involving unary and binary operators.
-- We have to deal with eq and neq0 separately, and also the order
-- comaprisons.

operExpr :: forall f a. (TextField f, KnownUni f a) => Parser (Expr f a)
operExpr = makeExprParser expr1 operators

operators :: forall f a. KnownUni f a => [[Comb.Operator Parser (Expr f a)]]
operators = case knownUni @f @a of
    -- The order here determines operator precedence.
    Bool   ->
        [ [Prefix (EAppUnOp  Not <$ keyword "not")]
        , [InfixL (EAppBinOp Xor <$ keyword "xor")]
        , [InfixL (EAppBinOp And <$ keyword "and")]
        , [InfixL (EAppBinOp Or  <$ keyword "or")]
        ]
    Field  ->
        [ [Prefix (EAppUnOp  Neg <$ keyword "neg"), Prefix (EAppUnOp Inv <$ keyword "inv")]
        , [InfixL (EAppBinOp Mul <$ symbol "*"), InfixL (EAppBinOp Div <$ symbol "/")]
        , [InfixL (EAppBinOp Add <$ symbol "+"), InfixL (EAppBinOp Sub <$ symbol "-")]
        ]
    Vector -> []

-- Can we somehow commit to an if-expression when we see "if expr_B" and then
-- continue with the other cases?  This would reduce the need for backtracking.
-- Probably not, because we don't know the type of the entire expression until
-- we see the first branch.

ifExprPoly :: (TextField f, KnownUni f a) => Parser (Expr f a)
ifExprPoly = EIf
    <$> (keyword "if"   *> exprPoly)
    <*> (keyword "then" *> exprPoly)
    <*> (keyword "else" *> exprPoly)

batExprPoly :: TextField f => Parser (Expr f Bool)
batExprPoly = flip (EAppBinOp BAt)
    <$> exprPoly
    <*> (symbol "[" *> exprPoly <* symbol "]")

estatementPoly :: (TextField f, KnownUni f a) => Parser (Expr f a)
estatementPoly = flip (foldr EStatement) <$> statements <*> exprPoly

statements :: TextField f => Parser [Statement f]
statements = fmap concat . some $ asum
    [ pure <$> assertStatement
    , pure <$> letStatement
    , forStatements
    ] <* symbol ";"

assertStatement :: TextField f => Parser (Statement f)
assertStatement = EAssert <$> (keyword "assert" *> exprPoly)

letStatement :: TextField f => Parser (Statement f)
letStatement = do
    -- The type of the variable determines how to parse the body of the let-expression.
    Some (var@(UniVar varUni _)) <- keyword "let" *> varSome
    withKnownUni varUni $ ELet var <$> (symbol "=" *> exprPoly)

unrollLoop :: Field f => Var -> Integer -> Integer -> [Statement f] -> [Statement f]
unrollLoop var lower bound stats = do
    i <- [lower .. bound]
    map (instStatement $ insertVar var (Some $ fromInteger i) mempty) stats

forStatements :: TextField f => Parser [Statement f]
forStatements = unrollLoop
    <$> (keyword "for" *> var_F)
    <*> (symbol  "="   *> signedDecimal)
    <*> (keyword "to"  *> signedDecimal)
    <*> (keyword "do"  *> statements)
    <* keyword "end"
