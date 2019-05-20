{-# OPTIONS_GHC -fno-warn-orphans #-}

module TinyLang.Boolean.Generator
    (
     boundedAbritraryExpr,
     defaultArbitraryExpr,
     ExprWithEnv (..),
     nodes,
     depth
    ) where

import           TinyLang.Boolean.Core
import           TinyLang.Environment
import           TinyLang.Generator
import           TinyLang.Var

import qualified Data.IntMap.Strict    as IntMap
import           Test.QuickCheck

-- | Generator for values
arbitraryValue :: Gen Bool
arbitraryValue = arbitrary

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
