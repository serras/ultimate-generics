{-# language KindSignatures         #-}
{-# language MultiParamTypeClasses  #-}
{-# language FunctionalDependencies #-}
{-# language TypeFamilies           #-}
{-# language TypeInType             #-}
{-# language ExplicitForAll         #-}
{-# language TypeOperators          #-}
{-# language FlexibleContexts       #-}
{-# language UndecidableInstances   #-}
module Generics.Ultimate.Class where

import Data.Kind (Constraint)
import GHC.TypeLits (TypeError, ErrorMessage(..))

import Data.Promoted.Nat
import Data.Promoted.List
import Generics.Ultimate.Code
import Generics.Ultimate.ListOfTypes
import Generics.Ultimate.Interpretation

class GenericFamily dk (fs :: [dk]) | fs -> dk where
  type Codes fs :: Family dtk
  from :: El dk fs n tys -> SOP dk (Codes fs) n fs tys
  to   :: (SSLoT dk tys)
       => SOP dk (Codes fs) n fs tys -> El dk fs n tys

class (GenericFamily dk '[f], Codes '[f] ~ '[code])
      => Generic (f :: dk) (code :: DataType dk) where

from1 :: Generic f code
      => ApplyT dk f tys -> NS (NB dk dk '[f] tys) code
from1 x = let SOP SZ y = from (El SZ x) in y

to1 :: (Generic f code, SSLoT dk tys)
    => NS (NB dk dk '[f] tys) code -> ApplyT dk f tys
to1 x = let El SZ y = to (SOP SZ x) in y

type family AllD c xs :: Constraint where
  AllD c '[] = ()
  AllD c (x ': xs) = (AllB c x, AllD c xs)

type family AllB c xs :: Constraint where
  AllB c (Constr x) = AllE c x

type family AllE c xs :: Constraint where
  AllE c '[] = ()
  AllE c (Explicit x ': xs) = (c x, AllE c xs)