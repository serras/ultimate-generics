{-# language TypeOperators         #-}
{-# language DataKinds             #-}
{-# language ExplicitNamespaces    #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances     #-}
{-# language TypeFamilies          #-}
{-# language MagicHash             #-}
{-# language TypeApplications      #-}
{-# language TypeInType            #-}
module Generics.Ultimate.Examples.List where

import Generics.Ultimate

instance Generic [] where
  type Code [] = '[ Constr '[], Constr '[ Explicit V0, Explicit (R0 :@: V0) ] ]

  from (A# (A0 [])) = Here $ Cr $ Nil
  from (A# (A0 (x : xs))) = There $ Here $ Cr $ E @_ @_ @V0 x :* E xs :* Nil
  
  to = to' sslot
    where
      to' :: SLoT (* -> *) tys
          -> Rep (* -> *) (Code []) [] tys
          -> ApplyT (* -> *) [] tys
      to' (SLoTA SLoT0) (Here (Cr Nil))
        = A# (A0 [])
      to' (SLoTA SLoT0) (There (Here (Cr (E x :* E xs :* Nil))))
        = A# (A0 (x : xs))
