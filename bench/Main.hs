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


box :: (KnownUni f a) => Expr f a -> SomeUniExpr f
box expr = SomeOf knownUni expr

unbox :: forall f a. (KnownUni f a) => SomeUniExpr f -> Expr f a
unbox (SomeOf uni expr) = withGeqUni uni (knownUni :: Uni f a) (error message) expr where
    message = "Uni mismatch!"

boxF :: (KnownUni f a, Functor t) => t (Expr f a) -> t (SomeUniExpr f)
boxF = fmap box

unboxF :: (KnownUni f a, Functor t) => t (SomeUniExpr f) -> t (Expr f a)
unboxF = fmap unbox

wrapF :: (KnownUni f a, KnownUni f b, Functor t) => (SomeUniExpr f -> t (SomeUniExpr f)) -> Expr f a -> t (Expr f b)
wrapF f = unboxF . f . box


progSubExpr :: Traversal' (Program f) (SomeUniExpr f)
progSubExpr f = \case
    Program exts stmts ->
        Program exts <$> stmtsSubExpr f stmts
stmtsSubExpr :: Traversal' (Statements f) (SomeUniExpr f)
stmtsSubExpr f = \case
    Statements stmts ->
        Statements <$> traverse (stmtSubExpr f) stmts

stmtSubExpr :: forall f. Traversal' (Statement f) (SomeUniExpr f)
stmtSubExpr f = \case
    ELet uniVar@(UniVar uni _) expr ->
        withKnownUni uni $
            ELet uniVar <$> wrapF f expr
    EAssert expr ->
        EAssert <$> wrapF f expr

exprSubExpr :: Traversal' (SomeUniExpr f) (SomeUniExpr f)
exprSubExpr f = \case
    SomeOf uni e0 -> SomeOf uni <$> case e0 of
        EAppUnOp unOp e ->
            withKnownUni (uniOfExpr e) $
            EAppUnOp unOp <$> wrapF f e
        EAppBinOp binOp e1 e2 ->
            withKnownUni (uniOfExpr e1) $
            withKnownUni (uniOfExpr e2) $
                EAppBinOp binOp <$> wrapF f e1 <*> wrapF f e2
        EIf e e1 e2 ->
            withKnownUni (uniOfExpr e)  $
            withKnownUni (uniOfExpr e1) $
            withKnownUni (uniOfExpr e2) $
                EIf <$> wrapF f e <*> wrapF f e1 <*> wrapF f e2
        x -> pure x

-- TODO:  Count external variables as well
progNodes :: Program f -> Sum Int
progNodes prog = foldMapOf progSubStatement stmtNodes prog

stmtsNodes :: Statements f -> Sum Int
stmtsNodes stmts = foldMapOf stmtsSubStatement stmtNodes stmts

stmtNodes :: Statement f -> Sum Int
stmtNodes stmt = Sum 1 <> foldMapOf stmtSubExpr exprNodes stmt

exprNodes :: SomeUniExpr f -> Sum Int
exprNodes e = Sum 1 <> foldMapOf exprSubExpr exprNodes e

-- NOTE:  We need Max 0 for the empty case
progDepth :: Program f -> Max Int
progDepth = stmtsDepth . _programStatements

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
