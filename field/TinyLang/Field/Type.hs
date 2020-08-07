module TinyLang.Field.Type
    ( Type(..)
    , pattern UniType
    , pattern Bool
    , pattern Field
    , pattern Vector
    ) where

import           Data.Field
import qualified TinyLang.Field.Uni         as U
import           TinyLang.Field.Existential

-- Types
data Type f
    = BuiltIn (Some (TypeIn U.Uni f))
    | TyFun   (Type f)          (Type f)


pattern UniType :: U.Uni f a -> Type f
pattern UniType uni = BuiltIn (Some (TypeIn uni))

deriving instance (TextField f) => Show (Type f)
deriving instance Eq  (Type f)
deriving instance Ord (Type f)

pattern Bool :: Type f
pattern Bool = UniType U.Bool

pattern Field :: Type f
pattern Field = UniType U.Field

pattern Vector :: Type f
pattern Vector = UniType U.Vector

newtype TypeIn uni f a = TypeIn (uni f a)
    deriving newtype (Show, Eq, Ord)

instance Eq (Some (TypeIn U.Uni f)) where
    Some (TypeIn u1) == Some (TypeIn u2) = Some u1 == Some u2

instance Ord (Some (TypeIn U.Uni f)) where
    Some (TypeIn u1) `compare` Some (TypeIn u2) = (Some u1) `compare` (Some u2)

deriving instance (TextField f) => Show (Some (TypeIn U.Uni f))
