module TinyLang.Field.Typed.TypeChecker
    where

-- import           TinyLang.Prelude

-- import           Data.Field
import qualified TinyLang.Field.Typed.Core as T
import qualified TinyLang.Field.Raw.Core   as R

typeCheck :: forall f v a. R.Expr v f -> T.Expr f a
typeCheck = undefined
