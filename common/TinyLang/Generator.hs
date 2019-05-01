{-# OPTIONS_GHC -fno-warn-orphans #-}

module TinyLang.Generator
    (
     makeVars,
     defaultVars,
     arbitraryVar
    ) where

import           TinyLang.Environment
import           TinyLang.Var

import qualified Data.IntMap.Strict   as IntMap
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

-- | Generator for variables, choosing from the given list.
arbitraryVar :: [Var] -> Gen Var
arbitraryVar = elements

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
