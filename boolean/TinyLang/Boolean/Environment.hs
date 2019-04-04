module TinyLang.Boolean.Environment
where

import           TinyLang.Var

-- | A simple representation of environments as association lists mapping variables to values
type Env = [(Var, Bool)]

lookupVar :: Var -> Env -> Bool
lookupVar v []        = error $ "Variable " ++ show v ++ " not found in environment"
lookupVar v ((w,b):l) = if v==w then b else lookupVar v l
