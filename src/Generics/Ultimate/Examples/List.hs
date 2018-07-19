{-# language TypeOperators         #-}
{-# language DataKinds             #-}
{-# language TypeFamilies          #-}
{-# language MagicHash             #-}
{-# language TypeApplications      #-}
{-# language TypeInType            #-}
{-# language ScopedTypeVariables   #-}
{-# language InstanceSigs          #-}
{-# language FlexibleContexts      #-}
module Generics.Ultimate.Examples.List where

import Generics.Ultimate
import Generics.Ultimate.Derive.KFunctor

instance Generic [] where
  type Code [] = '[ '[], '[ Value V0, Value (Kon [] :@: V0) ] ]

  from (A# (A0 [])) = Here $ Nil
  from (A# (A0 (x : xs))) = There $ Here $ V x :* V xs :* Nil
  
  to :: forall tys. SSLoT (* -> *) tys
     => SOP (* -> *) (Code []) tys -> ApplyT (* -> *) [] tys
  to x = case sslot @_ @tys of
    SLoTA SLoT0 -> case x of
      Here Nil -> A# (A0 [])
      There (Here (V x :* V xs :* Nil))  -> A# (A0 (x : xs))

instance KFunctor []