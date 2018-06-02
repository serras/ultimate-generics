{-# language KindSignatures         #-}
{-# language MultiParamTypeClasses  #-}
{-# language FunctionalDependencies #-}
{-# language TypeFamilies           #-}
{-# language TypeInType             #-}
{-# language ExplicitForAll         #-}
{-# language TypeOperators          #-}
module Generics.Ultimate.Class where

import Data.Kind (Constraint)

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

type family AllD c xs :: Constraint where
  AllD c '[] = ()
  AllD c (x ': xs) = (AllB c x, AllD c xs)

type family AllB c xs :: Constraint where
  AllB c (Constr x) = AllE c x

type family AllE c xs :: Constraint where
  AllE c '[] = ()
  AllE c (Explicit x ': xs) = (c x, AllE c xs)