{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           TinyLang.Field.Generator  ()
import           TinyLang.Field.Typed.Core

import           Control.Lens
import           Control.Monad
import           Data.List
import           Data.Semigroup
import           Test.QuickCheck

progNodes :: Program f -> Sum Int
progNodes prog =
    foldMapOf progSubExt (const (Sum 1)) prog
    <> foldMapOf progSubStatements stmtsNodes prog

stmtsNodes :: Statements f -> Sum Int
stmtsNodes stmts = foldMapOf stmtsSubStatement stmtNodes stmts

stmtNodes :: Statement f -> Sum Int
stmtNodes stmt = Sum 1 <> foldMapOf stmtSubExpr exprNodes stmt

exprNodes :: SomeUniExpr f -> Sum Int
exprNodes e = Sum 1 <> foldMapOf exprSubExpr exprNodes e

-- NOTE:  We need Max 0 for the empty case
progDepth :: Program f -> Max Int
progDepth prog =
    Max 0
    <> foldMapOf progSubExt (const (Max 1)) prog
    <> foldMapOf progSubStatements stmtsDepth prog

stmtsDepth :: Statements f -> Max Int
stmtsDepth stmts = Max 0 <> foldMapOf stmtsSubStatement stmtDepth stmts

stmtDepth :: Statement f -> Max Int
stmtDepth stmt = (+1) <$> Max 0 <> foldMapOf stmtSubExpr exprDepth stmt

exprDepth :: SomeUniExpr f -> Max Int
exprDepth expr = (+1) <$> Max 0 <>  foldMapOf exprSubExpr exprDepth expr

data TestResult = TestResult { nodes :: Int
                             , depth :: Int
                             }
                  deriving (Show)

runGen :: Int -> IO TestResult
runGen size = do
    prog <- generate (resize size arbitrary) :: IO (Program (AField Rational))
    pure $ TestResult (getSum (progNodes prog)) (getMax (progDepth prog))

average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / genericLength xs

main :: IO ()
main = do
    let size = 1000
    let runs = 1000 :: Int
    putStrLn $ "Requested runs:  " ++ show runs
    putStrLn $ "Requested size:  " ++ show size
    results <- forM [1 .. runs] $ \_ -> runGen size
    let nodess = map nodes results
    let depths = map depth results
    let minn = minimum nodess
    let maxn = maximum nodess
    let avgn = average nodess :: Double
    let maxd = maximum depths
    let mind = minimum depths
    let avgd = average depths :: Double
    putStrLn ""
    putStrLn $ "Minimum depth = " ++ show mind
    putStrLn $ "Maximum depth = " ++ show maxd
    putStrLn $ "Mean depth    = " ++ show avgd
    putStrLn ""
    putStrLn $ "Minimum number of nodes = " ++ show minn
    putStrLn $ "Maximum number of nodes = " ++ show maxn
    putStrLn $ "Mean number of nodes    = " ++ show avgn
    putStrLn ""
