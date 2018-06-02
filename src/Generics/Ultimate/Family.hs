{-# language DataKinds              #-}
{-# language GADTs                  #-}
{-# language TypeOperators          #-}
{-# language TypeFamilies           #-}
{-# language PolyKinds              #-}
{-# language KindSignatures         #-}
{-# language TypeInType             #-}
{-# language ExplicitForAll         #-}
{-# language MultiParamTypeClasses  #-}
{-# language FunctionalDependencies #-}
module Generics.Ultimate.Family where

import Data.Promoted.Nat
import Data.Promoted.List
import Generics.Ultimate.Code
import Generics.Ultimate.ListOfTypes
import Generics.Ultimate.Interpretation

data El k (fs :: [k]) (n :: Nat) (tys :: LoT k) where
  El :: SNat n -> ApplyT k (Lkp n fs) tys -> El k fs n tys

unravelEl :: El k fs n tys -> Apply k (Lkp n fs) tys
unravelEl (El _ x) = unravel x

ravelEl :: forall k fs n tys.
           (SSLoT k tys, SSNat n)
        => Apply k (Lkp n fs) tys -> El k fs n tys
ravelEl x = El ssnat (ravel x)

class GenericFamily dk (fs :: [dk]) | fs -> dk where
  type Codes fs :: Family dtk
  from :: El dk fs n tys -> SOP dk (Codes fs) n fs tys
  to   :: (SSLoT dk tys)
       => SOP dk (Codes fs) n fs tys -> El dk fs n tys
