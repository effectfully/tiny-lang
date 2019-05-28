{-| A parser for the boolean language.  The concrete syntax is as follows:

  val ::= T | F
  fvar ::= [a-z][a-z0-9_]*
  bvar ::=  '?'[a-z][a-z0-9_]*

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
           expr + expr
           expr - expr
           expr * expr
           expr / expr
           'if' expr 'then' expr 'else' expr
           (expr)

  Things like 'and' denote keywords.

  Precedence: 'not' > 'xor' > 'and' > 'or'  (but use parentheses anyway).
  if-then-else has to be parenthesised unless it's at the very top.

  FIXME: what about numeric operators?

  The code is based on the tutorial at
  https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html

  See also https://markkarpov.com/megaparsec/megaparsec.html
-}

module TinyLang.Field.Parser
    ( parseExpr,
      ExprWrapper(..)
    ) where

import           TinyLang.Field.Core
--import           TinyLang.Field.Printer
import           TinyLang.Prelude               hiding (many, try)
import           TinyLang.Var

import           Control.Applicative (pure)
import           Control.Monad.Combinators.Expr as E
import qualified Data.Map                       as M
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           TinyLang.Field.ParsableField    
import           TinyLang.Field.ParserUtils

-- A uniform type to contain exprs of type bool/numeric.  Maybe this
-- could go in Core.hs;  do we really need it though?
data ExprWrapper f =
    BoolExpr (Expr f Bool)
  | FieldExpr (Expr f (AField f))
    deriving (Show)

-- | Look up a variable name. If we've already seen it, return the corresponding Var;
-- otherwise, increase the Unique counter and use it to construct a new Var.
makeVar :: (MonadState IdentifierState m) => String -> m Var
makeVar name = do
    (ss, counter) <- get
    case M.lookup name ss of
        Just v -> pure v
        Nothing -> do
            let counter' = counter + 1
                v = Var (Unique counter') name
            put (M.insert name v ss, counter')
            pure v

-- | The main entry point: parse a string and return Either an error message or an Expr.
parseExpr :: ParsableField f => String -> Either String (ExprWrapper f)
parseExpr s = first errorBundlePretty . fst $ runState (runParserT top "" s) emptyIdentifierState

-- Parse the whole of an input stream
top :: ParsableField f => Parser (ExprWrapper f)
top = between ws eof expr

expr :: ParsableField f => Parser (ExprWrapper f)
expr = (try (BoolExpr <$> expr_B)) <|> (FieldExpr <$> expr_F)
-- ^ Putting FieldExpr first causes trouble with non-parenthesised "1==2", for example.
-- I'm not sure why: it seems to see the 1 and then starts parsing a field expression,
-- but it should backtrack when it fails.  Maybe makeExprParser doesn't backtrack enough?
      
-- Keywords
keywords :: [String]
keywords = ["T", "F", "not", "and", "or", "xor", "if", "then", "else", "neq0", "neg", "inv"]

-- Parse a keyword, checking that it's not a prefix of something else
keyword :: String -> Parser ()
keyword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)


-- Most of the remanining parsers have a B or F suffix depending on
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
identifier_B =  (lexeme . try) (p >>= check)
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
valExpr_F :: forall f . ParsableField f => Parser (Expr f (AField f))
valExpr_F = (\v -> EVal (UniVal Field v)) <$> (parseFieldElement :: Parser (AField f))
          
-- Variables
varExpr_F :: ParsableField f => Parser (Expr f (AField f))
varExpr_F = EVar Field <$> (identifier_F >>= makeVar)

varExpr_B :: Parser (Expr f Bool)
varExpr_B = EVar Bool <$> (identifier_B >>= makeVar)

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
expr1_B =  try eqExpr <|> try valExpr_B <|> try varExpr_B <|> try neq0Expr <|> parens expr_B

expr1_F :: ParsableField f => Parser (Expr f (AField f))
expr1_F =  try varExpr_F <|> try valExpr_F <|> parens expr_F

-- Special cases for eq and neq0 because the return type isn't the
-- same as the argument type(s).
neq0Expr :: ParsableField f => Parser (Expr f Bool)
neq0Expr = EAppUnOp Neq0 <$ keyword "neq0" <*> expr_F

eqExpr :: ParsableField f => Parser (Expr f Bool)
eqExpr = EAppBinOp FEq <$>  expr_F <* symbol "==" <*> expr_F

-- expr: full expressions
expr_B :: ParsableField f => Parser (Expr f Bool)
expr_B = try eqExpr <|> try operExpr_B <|> ifExpr_B

expr_F :: ParsableField f => Parser (Expr f (AField f))
expr_F = (try operExpr_F) <|> ifExpr_F 

         
-- operExpr: expressions involving unary and binary operators
-- We have to deal with eq and neq0 separately.

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

operators_F ::[[E.Operator Parser (Expr f (AField f))]]
operators_F = -- The order here determines operator precedence.
  [ [Prefix (EAppUnOp  Neg <$ keyword "neg"), Prefix (EAppUnOp Inv <$ keyword "inv")]
  , [InfixL (EAppBinOp Mul <$ symbol "*"), InfixL (EAppBinOp Div <$ symbol "/")]
  , [InfixL (EAppBinOp Add <$ symbol "+"), InfixL (EAppBinOp Sub <$ symbol "-")]
  ]
             

-- if e then r1 else e2

-- Branches are boolean
ifExpr_B :: ParsableField f => Parser (Expr f Bool)
ifExpr_B = EIf <$> (keyword "if" *> expr_B) <*> (keyword "then" *> expr_B) <*> (keyword "else" *> expr_B)

-- Branches are numeric
ifExpr_F :: ParsableField f => Parser (Expr f (AField f))
ifExpr_F = EIf <$> (keyword "if" *> expr_B) <*> (keyword "then" *> expr_F) <*> (keyword "else" *> expr_F)
