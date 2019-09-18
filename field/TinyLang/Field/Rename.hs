module TinyLang.Field.Rename
    ( renameExpr
    ) where

import           TinyLang.Prelude

import           TinyLang.Field.Core

renameExpr :: MonadSupply m => Expr f a -> m (Expr f a)
renameExpr expr = do
    supplyFromAtLeastFree expr
    runRenameM $ renameExprM expr

type RenameM = ReaderT (Env Unique) Supply

runRenameM :: MonadSupply m => RenameM a -> m a
runRenameM a = liftSupply $ runReaderT a mempty

withFreshenedVar :: Var -> (Var -> RenameM c) -> RenameM c
withFreshenedVar var cont = do
    uniqNew <- freshUnique
    local (insertVar var uniqNew) . cont $ setUnique uniqNew var

renameVarM :: Var -> RenameM Var
renameVarM var = do
    mayUniq <- asks $ lookupVar var
    pure $ case mayUniq of
        Nothing   -> var
        Just uniq -> setUnique uniq var

withRenamedStatementM :: Statement f -> (Statement f -> RenameM c) -> RenameM c
withRenamedStatementM (ELet (UniVar uni var) def) cont = do
    defRen <- renameExprM def
    withFreshenedVar var $ \varFr -> cont $ ELet (UniVar uni varFr) defRen
withRenamedStatementM (EAssert expr)              cont = renameExprM expr >>= cont . EAssert

renameExprM :: Expr f a -> RenameM (Expr f a)
renameExprM (EVal uniVal)              = pure $ EVal uniVal
renameExprM (EVar (UniVar uni var))    = EVar . UniVar uni <$> renameVarM var
renameExprM (EIf cond expr1 expr2)     =
    EIf <$> renameExprM cond <*> renameExprM expr1 <*> renameExprM expr2
renameExprM (EAppUnOp op expr)         = EAppUnOp op <$> renameExprM expr
renameExprM (EAppBinOp op expr1 expr2) = EAppBinOp op <$> renameExprM expr1 <*> renameExprM expr2
renameExprM (EStatement stat expr)     =
    withRenamedStatementM stat $ \statRen -> EStatement statRen <$> renameExprM expr
