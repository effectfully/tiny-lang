module TinyLang.Boolean.Generator
    (
     makeVars,
     defaultVars,
     boundedAbritraryExpr,
     defaultArbitraryExpr,
     prop_checkparse
    ) where

import           Control.Monad
import           Test.QuickCheck
import           TinyLang.Boolean.Core
import           TinyLang.Boolean.Parser
import           TinyLang.Boolean.Printer
import           TinyLang.Var


{-|
  We can't use String as the type of variable names in generators for
  expressions, because then it's probable that the variables occurring
  in a randomly generated expression will all be distinct, which is
  unrealistic. Also we have to conform to the concrete syntax, and
  deal sensibly with Unique IDs in some way.

  To deal with these issues we parameterise the expression generator
  over a list of Vars, and the variables appearing in the expression
  will be chosen uniformly from this list using QuickCheck's
  'elements'.  It's OK to have repeated variables: the more often a
  variable appears in the list, the more often it's likely to appear
  in a random expression.

  Variable names should be of the form [a-z][a-z0-9]* if they're going
  to be printed and fed to the parser.
-}

type VarName = String

-- | A convenience method to convert a list of Strings into a list of
-- Vars. The variables are given unique serial numbers 1,2,3,...,
-- which means that multiple occurrences of the same name will yield
-- different Vars: this may or may not be what you want.
makeVars :: [VarName] -> [Var]
makeVars = zipWith (\index name -> Var (Unique index) name) [1..]

-- | A list of default variables for convenience.
defaultVars :: [Var]
defaultVars = makeVars ["a", "b", "c", "d", "e", "f", "g", "h"]

-- | Default depth bound for generated expressions.
defaultDepth :: Int
defaultDepth = 6

-- | Generator for values
arbitraryValue :: Gen Bool
arbitraryValue = arbitrary

-- | Generator for variables, choosing from the given list.
arbitraryVar :: [Var] -> Gen Var
arbitraryVar = elements

-- | Generator for unary operators.  We only have one at the moment,
-- so this is kind of trivial: it's easily extensible though.
arbitraryUnOp :: Gen UnOp
arbitraryUnOp = elements [Not]

-- | Generator for binary operators.
arbitraryBinOp :: Gen BinOp
arbitraryBinOp = elements [And, Xor, Or]

-- Generator for atoms (expressions with no subexpressions): EVal or EVar.
arbitraryAtom :: [Var] -> Gen Expr
arbitraryAtom vars = oneof [liftM  EVal arbitraryValue, liftM  EVar (arbitraryVar vars)]


{-| Generate an arbirary expression of maximum depth d.  If we use
    'elements' to choose uniformly from the five constructors of Expr
    then 40% of the time we get an atom so the trees aren't very deep
    (indeed, 40% of the time they only have a single node). We use
    'frequency' instead, and the chosen frequencies give us an atom
    twice out of every 17 atttempts, which make the trees quite deep
    and bushy.

    Depending on the use case, it might be worth adjusting the frequencies.
    We might also try to bond the number of nodes, rather than the depth.
-}
-- | NOTE: we should probably use 'sized' here: it'll help QuickCheck to search for counterexamples.
boundedAbritraryExpr :: [Var] -> Int -> Gen Expr
boundedAbritraryExpr vars depth =
    if depth == 0 then
        arbitraryAtom vars
    else
        frequency [ (1, EVal      <$> arbitraryValue)
                  , (1, EVar      <$> arbitraryVar vars)
                  , (5, EAppUnOp  <$> arbitraryUnOp <*> subexpr)
                  , (5, EAppBinOp <$> arbitraryBinOp <*> subexpr <*> subexpr)
                  , (5, EIf       <$> subexpr <*> subexpr <*> subexpr)
                  ]
        where subexpr = boundedAbritraryExpr vars (depth-1)


-- | A default generator: variables a..h, depth 10
defaultArbitraryExpr :: Gen Expr
defaultArbitraryExpr = boundedAbritraryExpr defaultVars defaultDepth

{- A simple shrinker.  If we get a failing example then it just tries
 all of the subexpressions. This will produce a minimal example if,
 for example we do something like printing with Unique IDs but
 parsing without them (because our test compares names but not IDs,
 so we'll be getting things like "b_2" == "b").  If this isn't good
 enough, we could also try things like replacing individual leaves
 with atoms in failing cases.
-}
shrinkExpr :: Expr -> [Expr]
shrinkExpr (EAppUnOp op e)      = [e]
shrinkExpr (EAppBinOp op e1 e2) = [e1, e2]
shrinkExpr (EIf e e1 e2)        = [e,e1,e2]
shrinkExpr (EVal _)             = []  -- Can't shrink an atom
shrinkExpr (EVar _)             = []


-- Do we really want to define an instance here?  I couldn't
-- immediately see any other way to run quickCheck with a specified
-- generator and shrinker, but I probably didn't look closely enough.
instance Arbitrary Expr
    where arbitrary = defaultArbitraryExpr
          shrink = shrinkExpr

---------------------------------------------------------------------------
-- Since we've got a generator, let's use it to test the parser and printer.

-- We want to check that printing then parsing is the identity, but in
-- general it won't be because the Uniques in the variables will change.
-- Let's get round that by setting all the IDs to 0.
-- I'm sure there's a fancy Haskell way to do this, but it's not hard to
-- do it the old-fashioned way.

forgetID :: Var -> Var
forgetID v = Var (Unique 0) (_varName v)

forgetIDs :: Expr -> Expr
forgetIDs (EVal b)             = EVal b
forgetIDs (EVar v)             = EVar (forgetID v)
forgetIDs (EAppUnOp op e)      = EAppUnOp op (forgetIDs e)
forgetIDs (EAppBinOp op e1 e2) = EAppBinOp op (forgetIDs e1) (forgetIDs e2)
forgetIDs (EIf e e1 e2)        = EIf (forgetIDs e) (forgetIDs e1) (forgetIDs e2)

prop_checkparse e = let r = parseExpr (toStringWithIDs e)
                    in case r of
                         Left _  -> False
                         Right f -> forgetIDs f == forgetIDs e


