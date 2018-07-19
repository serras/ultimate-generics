{-# language DataKinds       #-}
{-# language ConstraintKinds #-}
{-# language TypeOperators   #-}
{-# language TypeFamilies    #-}
{-# language PolyKinds       #-}
{-# language TypeInType      #-}
module Generics.Ultimate.Constraints where

import Data.Kind (Constraint)

import Generics.Ultimate.Code
import Generics.Ultimate.ListOfTypes
import Generics.Ultimate.Interpretation

type family AllValuesD c xs :: Constraint where
  AllValuesD c '[] = ()
  AllValuesD c (x ': xs) = (AllValuesC c x, AllValuesD c xs)

type family AllValuesC c xs :: Constraint where
  AllValuesC c '[] = ()
  AllValuesC c (Value  x ': xs) = (c x, AllValuesC c xs)
  AllValuesC c (Constr x ': xs) =       AllValuesC c xs

type family SatisfyConstrD dk (tys :: LoT dk) xs :: Constraint where
  SatisfyConstrD dk tys '[] = ()
  SatisfyConstrD dk tys (x ': xs) = (SatisfyConstrC dk tys x, SatisfyConstrD dk tys xs)

type family SatisfyConstrC dk (tys :: LoT dk) xs :: Constraint where
  SatisfyConstrC dk tys '[] = ()
  SatisfyConstrC dk tys (Value  x ': xs) =               SatisfyConstrC dk tys xs
  SatisfyConstrC dk tys (Constr x ': xs) = (Ty dk tys x, SatisfyConstrC dk tys xs)