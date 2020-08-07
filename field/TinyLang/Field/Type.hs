module TinyLang.Field.Type
    ( Type(..)
    , UniType
    , pattern UniType
    , pattern Bool
    , pattern Field
    , pattern Vector
    ) where

import           Data.Field
import qualified TinyLang.Field.Uni         as U
import           TinyLang.Field.Existential

-- Types
data Type uni f
    = BuiltIn (Some (TypeIn uni f))
    | TyFun   (Type uni f)          (Type uni f)

type UniType = Type U.Uni

pattern UniType :: U.Uni f a -> UniType f
pattern UniType uni = BuiltIn (Some (TypeIn uni))

deriving instance (TextField f) => Show (UniType f)
deriving instance Eq  (UniType f)
deriving instance Ord (UniType f)

pattern Bool :: UniType f
pattern Bool = UniType U.Bool

pattern Field :: UniType f
pattern Field = UniType U.Field

pattern Vector :: UniType f
pattern Vector = UniType U.Vector

newtype TypeIn uni f a = TypeIn (uni f a)
    deriving newtype (Show, Eq, Ord)

instance Eq (Some (TypeIn U.Uni f)) where
    Some (TypeIn u1) == Some (TypeIn u2) = Some u1 == Some u2

instance Ord (Some (TypeIn U.Uni f)) where
    Some (TypeIn u1) `compare` Some (TypeIn u2) = (Some u1) `compare` (Some u2)

deriving instance (TextField f) => Show (Some (TypeIn U.Uni f))
