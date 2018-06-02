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

instance GenericFamily (* -> *) '[ [] ] where
  type Codes '[ [] ] = '[ '[ Constr '[], Constr '[ Explicit V0, Explicit (R0 :@: V0) ] ] ]

  from (El SZ (A# (A0 [])))
    = SOP SZ $ Here $ Cr $ Nil
  from (El SZ (A# (A0 (x : xs))))
    = SOP SZ $ There $ Here $ Cr $ E @_ @_ @V0 x :* E xs :* Nil
  
  to = to' sslot
    where
      to' :: SLoT (* -> *) tys
          -> SOP (* -> *) (Codes '[ [] ]) n '[ [] ] tys
          -> El  (* -> *) '[ [] ] n tys
      to' (SLoTA SLoT0) (SOP SZ (Here (Cr Nil)))
        = El SZ (A# (A0 []))
      to' (SLoTA SLoT0) (SOP SZ (There (Here (Cr (E x :* E xs :* Nil)))))
        = El SZ (A# (A0 (x : xs)))

instance Generic [] '[ Constr '[], Constr '[ Explicit V0, Explicit (R0 :@: V0) ] ]