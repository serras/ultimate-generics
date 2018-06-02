{-# language DataKinds       #-}
{-# language ConstraintKinds #-}
{-# language TypeOperators   #-}
{-# language TypeFamilies    #-}
{-# language PolyKinds       #-}
module Generics.Ultimate.Class where

import Data.Kind (Constraint)

import Generics.Ultimate.Code

type family AllD c xs :: Constraint where
  AllD c '[] = ()
  AllD c (x ': xs) = (AllB c x, AllD c xs)

type family AllB c xs :: Constraint where
  AllB c (Constr x) = AllE c x

type family AllE c xs :: Constraint where
  AllE c '[] = ()
  AllE c (Explicit x ': xs) = (c x, AllE c xs)
