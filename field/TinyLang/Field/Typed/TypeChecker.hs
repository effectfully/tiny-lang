{-| TypeChecker

Potential resources:
* https://www.cs.cmu.edu/~fp/courses/15312-f04/handouts/15-bidirectional.pdf
* http://www.cse.chalmers.se/~ulfn/papers/afp08/tutorial.pdf
* 
-}

module TinyLang.Field.Typed.TypeChecker
    where

-- import           TinyLang.Prelude

-- import           Data.Field
import qualified TinyLang.Field.Typed.Core as T
import qualified TinyLang.Field.Raw.Core   as R

typeCheck :: forall f v a. R.Expr v f -> T.Expr f a
typeCheck (R.EConst v)            = undefined
typeCheck (R.EVar x)              = undefined
typeCheck (R.EAppBinOp binOp m n) = undefined
typeCheck (R.EAppUnOp unOp m)     = undefined
typeCheck (R.EStatement s m)      = undefined
typeCheck (R.EIf l m n)           = undefined
