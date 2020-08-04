module TinyLang.Field.Rename
    ( renameProgram
    ) where

import           TinyLang.Prelude

import           TinyLang.Field.Typed.Core

import           Control.Monad.Cont
import           Control.Lens


renameProgram :: MonadSupply m => Program f -> m (Program f)
renameProgram prog = do
    -- NOTE:  We are not sure if we need to handle Programs with free variables.
    -- NOTE:  Compiler will run a renaming step before compilation anyways.
    progSupplyFromAtLeastFree prog
    renameProgram' prog

renameProgram' :: MonadSupply m => Program f -> m (Program f)
renameProgram' (Program exts stmts) = do
    runRenameM $
        runContT (traverse (ContT . withFreshenedSomeUniVar) exts) $ \ rVars ->
            withRenamedStatementsM stmts $ \ rStmts ->
                pure $ Program rVars rStmts

type RenameM = ReaderT (Env Unique) Supply

runRenameM :: MonadSupply m => RenameM a -> m a
runRenameM a = liftSupply $ runReaderT a mempty

withFreshenedSomeUniVar :: SomeUniVar f -> (SomeUniVar f -> RenameM c) -> RenameM c
withFreshenedSomeUniVar (Some uniVar) kont =
    withFreshenedUniVar uniVar $ kont . Some

-- NOTE:  Convenience method of wraping and unwrapping univar
withFreshenedUniVar :: UniVar f a -> (UniVar f a -> RenameM c) -> RenameM c
withFreshenedUniVar (UniVar uni var) kont =
    withFreshenedVar var $ kont . UniVar uni

withFreshenedVar :: Var -> (Var -> RenameM c) -> RenameM c
withFreshenedVar var kont = do
    uniqNew <- freshUnique
    local (insertVar var uniqNew) . kont $ set varUniq uniqNew var

renameVarM :: Var -> RenameM Var
renameVarM var = do
    mayUniq <- asks $ lookupVar var
    pure $ case mayUniq of
        Nothing   -> var
        Just uniq -> set varUniq uniq var

-- TODO:  Add unit tests for renaming
withRenamedStatementM :: Statement f -> (Statement f -> RenameM c) -> RenameM c
withRenamedStatementM (ELet uniVar def) kont = do
    defRen <- renameExprM def
    -- Note that @var@ is not in scope in @def@.
    withFreshenedUniVar uniVar $ \uniVarFr -> kont $ ELet uniVarFr defRen
withRenamedStatementM (EAssert expr) kont = renameExprM expr >>= kont . EAssert

withRenamedStatementsM :: Statements f -> (Statements f -> RenameM c) -> RenameM c
withRenamedStatementsM (Statements stmts) kont =
    runContT (traverse (ContT . withRenamedStatementM) stmts) $ kont . Statements

renameExprM :: Expr f a -> RenameM (Expr f a)
renameExprM (EConst uniConst)            = pure $ EConst uniConst
renameExprM (EVar (UniVar uni var))      = EVar . UniVar uni <$> renameVarM var
renameExprM (EIf cond expr1 expr2)       =
    EIf <$> renameExprM cond <*> renameExprM expr1 <*> renameExprM expr2
renameExprM (EAppUnOp unOp expr)           = EAppUnOp unOp <$> renameExprM expr
renameExprM (EAppBinOp binOp expr1 expr2)   = EAppBinOp binOp <$> renameExprM expr1 <*> renameExprM expr2
