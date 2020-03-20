{-| TypeChecker

Potential resources:
* https://www.cs.cmu.edu/~fp/courses/15312-f04/handouts/15-bidirectional.pdf
* http://www.cse.chalmers.se/~ulfn/papers/afp08/tutorial.pdf
* 
-}

{-

Recap the language:


-}

module TinyLang.Field.Typed.TypeChecker
    where

import           TinyLang.Prelude hiding (TypeError)

import           Data.Field
import           TinyLang.Environment
import           TinyLang.Var
import           TinyLang.ParseUtils
import           TinyLang.Field.Evaluator
import           TinyLang.Field.UniConst
import           TinyLang.Field.Existential
import qualified TinyLang.Field.Typed.Core  as T
import qualified TinyLang.Field.Raw.Core    as R

type MonadTypeError m = MonadError String m
type MonadTypeCheck m = ( MonadSupply m
                        , MonadScope m
                        , MonadTypeError m
                        )

withKnownUni :: Uni f a -> (KnownUni f a => c) -> c
withKnownUni Bool   = id
withKnownUni Field  = id
withKnownUni Vector = id

unrollLoop
    :: TextField f => T.Var -> Integer -> Integer -> [T.Statement f] -> [T.Statement f]
unrollLoop var lower bound stats = do
    i <- [lower .. bound]
    let env = insertVar var (Some $ fromInteger i) mempty
        report err = error $ "Panic: " ++ show err
    map (either report id . instStatement env) stats

{-| 
-}
mkSomeUniVar :: forall f. T.Var -> T.SomeUniVar f
mkSomeUniVar var
    | '?':_ <- _varName var = Some $ T.UniVar Bool   var
    | '#':_ <- _varName var = Some $ T.UniVar Vector var
    | otherwise             = Some $ T.UniVar Field  var

{-|
-}
mkUniVar :: forall f a m. (MonadTypeError m, KnownUni f a) => T.Var -> m (T.UniVar f a)
mkUniVar var =
    case mkSomeUniVar var of
        Some uniVar@(T.UniVar uni _) ->
            withGeqUniM uni (knownUni @f @a) "universe mismatch" uniVar

{-|
-}
typeSomeUniVar
    :: forall m f. (MonadSupply m, MonadScope m) => String -> m (T.SomeUniVar f)
typeSomeUniVar var = mkSomeUniVar <$> makeVar var

{-|
-}
typeUniVar
    :: forall m f a. (MonadTypeCheck m, KnownUni f a)
    => String -> m (T.UniVar f a)
typeUniVar var = mkUniVar =<< makeVar var

{-|
-}
typeExpr
    :: forall m f a. (MonadTypeCheck m, TextField f, KnownUni f a)
    => R.Expr String f -> m (T.Expr f a)
typeExpr (R.EConst (Some uniConst@(UniConst uni _))) =
    withGeqUniM uni
                (knownUni @f @a)
                "universe mismatch"
                (T.EConst uniConst)    
typeExpr (R.EVar var) = T.EVar <$> typeUniVar var
typeExpr (R.EAppBinOp binOp m n) = undefined
typeExpr (R.EAppUnOp unOp m) = undefined
typeExpr (R.EStatement s m) =
    flip (foldr T.EStatement) <$> typeStatement s
                              <*> typeExpr m
typeExpr (R.EIf l m n) =    
    T.EIf <$> typeExpr l
          <*> typeExpr m
          <*> typeExpr n

{-|
-}
typeStatement
    :: forall m f. (MonadTypeCheck m, TextField f)
    => R.Statement String f
    -> m [T.Statement f]
typeStatement (R.ELet var m) =
    do
        Some (uniVar@(T.UniVar uni _)) <- typeSomeUniVar var
        tStmt <- withKnownUni uni $ T.ELet uniVar <$> typeExpr m
        pure [tStmt]
typeStatement (R.EAssert m)  = pure . T.EAssert <$> typeExpr m
typeStatement (R.EFor var start end stmts) =
    do
        tVar <- makeVar var
        (unrollLoop tVar start end) . concat <$> mapM typeStatement stmts
