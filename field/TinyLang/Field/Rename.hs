{-# LANGUAGE ViewPatterns #-}

module TinyLang.Field.Rename
    ( renameProgram
    ) where

import           TinyLang.Prelude

import           TinyLang.Field.Typed.Core

import           Control.Monad.Cont

renameProgram :: MonadSupply m => Program f -> m (Program f)
renameProgram prog | stmts <- unProgram prog = do
    stmtsSupplyFromAtLeastFree stmts
    mkProgram <$> runRenameM (withRenamedStatementsM stmts pure)

type RenameM = ReaderT (Env Unique) Supply

runRenameM :: MonadSupply m => RenameM a -> m a
runRenameM a = liftSupply $ runReaderT a mempty

withFreshenedVar :: Var -> (Var -> RenameM c) -> RenameM c
withFreshenedVar var kont = do
    uniqNew <- freshUnique
    local (insertVar var uniqNew) . kont $ setUnique uniqNew var

renameVarM :: Var -> RenameM Var
renameVarM var = do
    mayUniq <- asks $ lookupVar var
    pure $ case mayUniq of
        Nothing   -> var
        Just uniq -> setUnique uniq var

-- TODO:  Add unit tests for renaming
withRenamedStatementM :: Statement f -> (Statement f -> RenameM c) -> RenameM c
withRenamedStatementM (ELet (UniVar uni var) def) kont = do
    defRen <- renameExprM def
    -- var is not in scope in def
    withFreshenedVar var $ \varFr -> kont $ ELet (UniVar uni varFr) defRen
withRenamedStatementM (EAssert expr) kont = renameExprM expr >>= kont . EAssert
withRenamedStatementM (EFor (UniVar uni var) start end stmts) kont =
    withFreshenedVar var $ \varFr ->
        withRenamedStatementsM stmts $ \stmtsRen ->
        -- NOTE: The language is imperative and we do not have lexical scoping,
        -- therefore kont is called inside @withRenamedStatementsM@.
            kont $ EFor (UniVar uni varFr) start end stmtsRen

withRenamedStatementsM :: Statements f -> (Statements f -> RenameM c) -> RenameM c
withRenamedStatementsM (unStatements -> stmts) kont =
    runContT (traverse (ContT . withRenamedStatementM) stmts) $ kont . mkStatements


renameExprM :: Expr f a -> RenameM (Expr f a)
renameExprM (EConst uniConst)            = pure $ EConst uniConst
renameExprM (EVar (UniVar uni var))      = EVar . UniVar uni <$> renameVarM var
renameExprM (EIf cond expr1 expr2)       =
    EIf <$> renameExprM cond <*> renameExprM expr1 <*> renameExprM expr2
renameExprM (EAppUnOp op expr)           = EAppUnOp op <$> renameExprM expr
renameExprM (EAppBinOp op expr1 expr2)   = EAppBinOp op <$> renameExprM expr1 <*> renameExprM expr2

