{-# language DataKinds          #-}
{-# language ConstraintKinds    #-}
{-# language ExplicitNamespaces #-}
{-# language TypeOperators      #-}
{-# language TypeFamilies       #-}
{-# language PolyKinds          #-}
{-# language KindSignatures     #-}
{-# language TypeInType         #-}
module Generics.Ultimate.Class where

import Generics.Ultimate.Code
import Generics.Ultimate.ListOfTypes
import Generics.Ultimate.Interpretation

class Generic (f :: dk) where
  type Code f :: DataType dk
  from :: ApplyT dk f tys -> SOP dk (Code f) tys
  to   :: SSLoT dk tys
       => SOP dk (Code f) tys -> ApplyT dk f tys
