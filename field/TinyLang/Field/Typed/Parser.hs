{-# OPTIONS_GHC -fno-warn-orphans #-}

{-| A parser for a tiny language involving booleans and field elements.
  The concrete syntax is as follows:

  val ::= T | F
  fvar ::= [a-z][a-z0-9_]*
  bvar ::=  '?'[a-z][a-z0-9_]*

  Note that boolean variable names must begin with '?' so that
  the parser knows what the type is.  We'd need environments
  or type annotations or something to avoid this.

  assertion ::=
      'assert' expr

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
      'unp' expr
      expr [expr]
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

module TinyLang.Field.Typed.Parser
    ( parseBy
    , parseScopedExpr
    , parseExpr
    ) where

import           TinyLang.Prelude               hiding (many, some, try, option)

import           Data.Field
import           TinyLang.Field.Raw.Parser
import           TinyLang.Field.Typed.Core
import           TinyLang.Field.Typed.TypeChecker
import           TinyLang.Field.Rename
import           TinyLang.ParseUtils

import qualified Data.IntMap.Strict             as IntMap
import qualified Data.IntSet                    as IntSet
import qualified Data.Map.Strict                as Map

-- TODO: use a proper @newtype@.
instance TextField f => IsString (Scoped (Some (Expr f))) where
    fromString = fmap (either error $ forget Some) . runSupply . parseScopedExpr

instance TextField f => IsString (Some (Expr f)) where
    fromString = _scopedValue <$> fromString

-- | Parse a @String@ and return @Either@ an error message or an @Expr@ of some type.
-- If the result is an error, then return the latest 'Scope', otherwise return the 'Scope'
-- consisting of all free variables of the expression.
parseScopedExpr
    :: forall f m. (MonadSupply m, TextField f)
    => String -> m (Scoped (Either String (SomeUniExpr f)))
parseScopedExpr str = do
    Scoped totalScope errorOrSomeUniExpr <- parseBy (pTop @f) str
    case errorOrSomeUniExpr of
        Left err -> return . Scoped totalScope $ Left err
        Right rawExpr -> do
            case typeCheck rawExpr of
                Left err' -> return . Scoped totalScope $ Left err'
                Right (SomeOf uni e) -> do
                    eRen <- renameExpr e
                    let freeIndices = IntMap.keysSet . unEnv $ exprFreeVarSigns eRen
                        isFree var = unUnique (_varUniq var) `IntSet.member` freeIndices
                        freeScope = Map.filter isFree totalScope
                    return . Scoped freeScope . Right $ SomeOf uni eRen
                    

-- | Parse a @String@ and return @Either@ an error message or an @Expr@ of some type.
parseExpr
    :: forall f m. (MonadSupply m, TextField f)
    => String -> m (Either String (SomeUniExpr f))
parseExpr = fmap _scopedValue . parseScopedExpr
