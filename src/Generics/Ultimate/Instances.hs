{-# language TypeOperators         #-}
{-# language DataKinds             #-}
{-# language TypeFamilies          #-}
{-# language MagicHash             #-}
{-# language TypeApplications      #-}
{-# language TypeInType            #-}
{-# language ScopedTypeVariables   #-}
{-# language InstanceSigs          #-}
{-# language FlexibleContexts      #-}
{-# language ExplicitNamespaces  #-}
module Generics.Ultimate.Instances where

import Data.Kind (type (*))

import Generics.Ultimate.Code
import Generics.Ultimate.ListOfTypes
import Generics.Ultimate.Interpretation
import Generics.Ultimate.Class

instance Generic [] where
  type Code [] = '[ '[], '[ Value V0, Value ([] :$: V0) ] ]

  from (A# (A0 [])) = Here $ Nil
  from (A# (A0 (x : xs))) = There $ Here $ V x :* V xs :* Nil
  
  to :: forall tys. SSLoT (* -> *) tys
     => SOP (* -> *) (Code []) tys -> ApplyT (* -> *) [] tys
  to x = case sslot @_ @tys of
    SLoTA SLoT0 -> case x of
      Here Nil -> A# (A0 [])
      There (Here (V x :* V xs :* Nil)) -> A# (A0 (x : xs))

instance Generic Maybe where
  type Code Maybe = '[ '[], '[ Value V0 ] ]

  from (A# (A0 Nothing)) = Here $ Nil
  from (A# (A0 (Just x))) = There $ Here $ V x :* Nil
  
  to :: forall tys. SSLoT (* -> *) tys
     => SOP (* -> *) (Code Maybe) tys -> ApplyT (* -> *) Maybe tys
  to x = case sslot @_ @tys of
    SLoTA SLoT0 -> case x of
      Here Nil -> A# (A0 Nothing)
      There (Here (V x :* Nil)) -> A# (A0 (Just x))

instance Generic Either where
  type Code Either = '[ '[ Value V0 ], '[ Value V1 ] ]

  from (A# (A# (A0 (Left  x)))) = Here $ V x :* Nil
  from (A# (A# (A0 (Right x)))) = There $ Here $ V x :* Nil
  
  to :: forall tys. SSLoT (* -> * -> *) tys
     => SOP (* -> * -> *) (Code Either) tys -> ApplyT (* -> * -> *) Either tys
  to x = case sslot @_ @tys of
    SLoTA (SLoTA SLoT0) -> case x of
      Here (V x :* Nil) -> A# (A# (A0 (Left x)))
      There (Here (V x :* Nil)) -> A# (A# (A0 (Right x)))