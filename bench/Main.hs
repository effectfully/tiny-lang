import           TinyLang.Field.Generator  ()
import           TinyLang.Field.Typed.Core

import           Control.Monad
import           Data.List
import           Test.QuickCheck

-- A couple of functions for checking the output of generators
progNodes :: Program f -> Int
progNodes = stmtsNodes . _programStatements

stmtsNodes :: Statements f -> Int
stmtsNodes = sum . map stmtNodes . unStatements

stmtNodes :: Statement f -> Int
stmtNodes (ELet _ e)         = 1 + exprNodes e
stmtNodes (EAssert e)        = 1 + exprNodes e

exprNodes :: Expr f a -> Int
exprNodes (EConst _)          = 1
exprNodes (EVar   _)          = 1
exprNodes (EAppUnOp _ e)      = 1 + exprNodes e
exprNodes (EAppBinOp _ e1 e2) = 1 + exprNodes e1 + exprNodes e2
exprNodes (EIf e e1 e2)       = 1 + exprNodes e + exprNodes e1 + exprNodes e2

progDepth :: Program f -> Int
progDepth = stmtsDepth . _programStatements

stmtsDepth :: Statements f -> Int
stmtsDepth = maximum . (0:) . map stmtDepth . unStatements

stmtDepth :: Statement f -> Int
stmtDepth (ELet _ e)         = 1 + exprDepth e
stmtDepth (EAssert e)        = 1 + exprDepth e

exprDepth :: Expr f a -> Int
exprDepth (EConst _)          = 1
exprDepth (EVar _)            = 1
exprDepth (EAppUnOp _ e)      = 1 + exprDepth e
exprDepth (EAppBinOp _ e1 e2) = 1 + max (exprDepth e1) (exprDepth e2)
exprDepth (EIf e e1 e2)       = 1 + max (exprDepth e)  (max (exprDepth e1) (exprDepth e2))

data TestResult = TestResult { nodes :: Int
                             , depth :: Int
                             }
                  deriving (Show)

runGen :: Int -> IO TestResult
runGen size = do
    prog <- generate (resize size arbitrary) :: IO (Program (AField Rational))
    pure $ TestResult (progNodes prog) (progDepth prog)

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
