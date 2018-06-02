{-# language DataKinds          #-}
{-# language ConstraintKinds    #-}
{-# language ExplicitNamespaces #-}
{-# language TypeOperators      #-}
{-# language TypeFamilies       #-}
{-# language PolyKinds          #-}
{-# language KindSignatures     #-}
{-# language TypeInType         #-}
module Generics.Ultimate.Single where

import Generics.Ultimate.Code
import Generics.Ultimate.ListOfTypes
import Generics.Ultimate.Interpretation

type Rep dk (dt :: DataType dk) (r :: dk) (tys :: LoT dk)
     = NS (NB dk dk '[r] tys) dt

class Generic (f :: dk) where
  type Code f :: DataType dk
  from :: ApplyT dk f tys -> Rep dk (Code f) f tys
  to   :: (SSLoT dk tys)
       => Rep dk (Code f) f tys -> ApplyT dk f tys
