{-# OPTIONS_GHC -fno-warn-orphans #-}

module TinyLang.Boolean.Generator
    (
     makeVars,
     defaultVars,
     boundedAbritraryExpr,
     defaultArbitraryExpr,
     ExprWithEnv (..),
     nodes,
     depth
    ) where

import           TinyLang.Boolean.Core
import           TinyLang.Boolean.Environment
import           TinyLang.Var

import qualified Data.IntMap.Strict           as IntMap
import           Test.QuickCheck

{-|
  We can't use String as the type of variable names in generators for
  expressions because then it's probable that the variables occurring
  in a randomly generated expression will all be distinct, which is
  unrealistic. Also we have to conform to the concrete syntax, and
  deal sensibly with Unique IDs in some way.

  To deal with these issues we parameterise the expression generator
  over a list of Vars, and the variables appearing in the expression
  will be chosen uniformly from this list using QuickCheck's
  'elements'.  It's OK to have repeated variables: the more often a
  variable appears in the list, the more often it's likely to appear
  in a random expression (but note that repeated Vars should be exactly
  the same, including the Uniqe ID).

  Variable names should be of the form [a-z][a-z0-9_]* if they're going
  to be printed and fed to the parser.
-}

type VarName = String

-- | A convenience method to convert a list of Strings into a list of
-- Vars. The variables are given unique serial numbers 0,1,2,...,
-- which means that multiple occurrences of the same name will yield
-- different Vars: this may or may not be what you want.
makeVars :: [VarName] -> [Var]
makeVars = zipWith (\index name -> Var (Unique index) name) [0..]

-- | A list of default variables for convenience.
defaultVars :: [Var]
defaultVars = makeVars ["a", "b", "c", "d", "e", "f", "g", "h"]

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

-- | Generator for atoms (expressions with no subexpressions): EVal or EVar.
arbitraryAtom :: [Var] -> Gen Expr
arbitraryAtom vars = oneof [EVal <$> arbitraryValue, EVar <$> (arbitraryVar vars)]


{-| Generate an arbiratry expression of maximum size 'size' containing
    the variables supplied in 'vars'.

    If we use 'elements' to choose uniformly from the five constructors of
    Expr then 40% of the time we get an atom so the trees aren't very
    deep (indeed, 40% of the time they only have a single node). We
    use 'frequency' instead, and the chosen frequencies give us an
    atom twice out of every 17 atttempts: this makes the trees quite
    deep and bushy.

    Depending on the use case, it might be worth adjusting the frequencies.

    Note that the 'size' parameter is an upper bound on the number of
    nodes, not the depth of the tree.  This works better with
    QuickCheck's handling of sizes.  For testing QuickCheck uses a
    default size of 100, and generating trees of depth 100 would be
    excessive.

    You probably won't get up to the size bound most of the time since
    that would require almost all of the nodes to be non-atoms.  Some
    quick tests suggest that if you supply a bound of 100,000 then
    samples are often of size 10000-35000 but seldom larger, so we're
    at least getting the right order of magnitude.

    To change the size while testing you can do things like

      quickCheckWith stdArgs {maxSize=10000} prop_checkparse

    See the QuickCheck documentation for the 'Args' type for
    more information.

-}

boundedAbritraryExpr :: [Var] -> Int -> Gen Expr
boundedAbritraryExpr vars size =
    if size <= 1 then
        arbitraryAtom vars
    else
        frequency [ (1, EVal      <$> arbitraryValue)
                  , (1, EVar      <$> arbitraryVar vars)
                  , (5, EAppUnOp  <$> arbitraryUnOp <*> subexpr1)
                  , (5, EAppBinOp <$> arbitraryBinOp <*> subexpr2 <*> subexpr2)
                  , (5, EIf       <$> subexpr3 <*> subexpr3 <*> subexpr3)
                  ]
            where subexpr1 = boundedAbritraryExpr vars (size-1)
                  subexpr2 = boundedAbritraryExpr vars (size `div` 2)
                  subexpr3 = boundedAbritraryExpr vars (size `div` 3)

-- | A default generator: defaultVars is defined above.
-- This is used in the Arbitrary instance for Expr below,
-- but you can also call it (or boundedAbritraryExpr) manually.
defaultArbitraryExpr :: Int -> Gen Expr
defaultArbitraryExpr = boundedAbritraryExpr defaultVars

{- A simple shrinker.  If we get a failing example then it just tries
 all of the subexpressions. This will produce a minimal example if,
 for example we do something like printing with Unique IDs but parsing
 without them (because our test compares names but not IDs, so we'll
 be getting things like "b_2" == "b").  If this isn't good enough, we
 could also try things like shrinking subexpressions or replacing
 individual subtrees with atoms in failing cases.
-}
shrinkExpr :: Expr -> [Expr]
shrinkExpr (EAppUnOp _ e)      = [e]
shrinkExpr (EAppBinOp _ e1 e2) = [e1, e2]
shrinkExpr (EIf e e1 e2)       = [e,e1,e2]
shrinkExpr (EVal _)            = []  -- Can't shrink an atom
shrinkExpr (EVar _)            = []


{-| An instance of Arbitrary for Expr.  QuickCheck will use this for
  testing properties.  In the QuickCheck API documentationI couldn't
  see any way to run tests using a specified generator and shrinker
  without using an instance , but maybe I didn't look closely enough.
  See the note about 'quickCheckWith' above for how to change the size
  bound while running tests.
-}
instance Arbitrary Expr
    where arbitrary = sized defaultArbitraryExpr
          shrink = shrinkExpr

-- Generates an arbitrary environment that
--
-- 1. likely contains values for some likely small prefix of 'defaultVars'
-- 2. contains values for some sublist of 'defaultVars'
-- 3. contains values for some arbitrary variables
instance Arbitrary a => Arbitrary (Env a) where
    arbitrary = do
        let len = length defaultVars
            varsToUniques = map (unUnique . _varUniq)
        prefixSize <- frequency $ map (\i -> (len - i, choose (0, i))) [0 .. len]
        let preUniques = varsToUniques $ take prefixSize defaultVars
        defUniques <- varsToUniques <$> sublistOf defaultVars
        arbUniques <- map abs <$> arbitrary
        let allUniques = preUniques ++ defUniques ++ arbUniques
        uniquesWithVars <- traverse (\i -> (,) i <$> arbitrary) allUniques
        return . Env $ IntMap.fromList uniquesWithVars

    shrink = map (Env . IntMap.fromList) . shrink . IntMap.toList . unEnv

data ExprWithEnv
    = ExprWithEnv Expr (Env Bool)
    deriving (Show, Eq)

instance Arbitrary ExprWithEnv where
    arbitrary = do
        -- Generate an expression.
        expr <- arbitrary
        -- Generate an arbitrary value for each variable in the generated expression.
        vals <- traverse (const arbitrary) $ exprVarNames expr
        return $ ExprWithEnv expr (Env vals)
    shrink (ExprWithEnv expr (Env vals)) =
        -- TODO: test me.
        flip map (shrink expr) $ \shrunkExpr ->
            ExprWithEnv shrunkExpr . Env . IntMap.intersection vals $ exprVarNames shrunkExpr

-- A couple of functions for checking the output of generators
nodes :: Expr -> Int
nodes (EVal _)            = 1
nodes (EVar _)            = 1
nodes (EAppUnOp _ e)      = 1 + nodes e
nodes (EAppBinOp _ e1 e2) = 1 + nodes e1 + nodes e2
nodes (EIf e e1 e2)       = 1 + nodes e + nodes e1 + nodes e2

depth :: Expr -> Int
depth (EVal _)            = 1
depth (EVar _)            = 1
depth (EAppUnOp _ e)      = 1 + depth e
depth (EAppBinOp _ e1 e2) = 1 + max (depth e1) (depth e2)
depth (EIf e e1 e2)       = 1 + max (depth e) (max (depth e1) (depth e2))
