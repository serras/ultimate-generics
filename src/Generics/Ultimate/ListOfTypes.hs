{-# language DataKinds              #-}
{-# language ExplicitNamespaces     #-}
{-# language TypeOperators          #-}
{-# language GADTs                  #-}
{-# language TypeFamilies           #-}
{-# language PolyKinds              #-}
{-# language KindSignatures         #-}
{-# language TypeInType             #-}
{-# language MultiParamTypeClasses  #-}
{-# language FunctionalDependencies #-}
{-# language TypeSynonymInstances   #-}
{-# language FlexibleInstances      #-}
{-# language FlexibleContexts       #-}
{-# language ScopedTypeVariables    #-}
{-# language TypeApplications       #-}
{-# language MagicHash              #-}
module Generics.Ultimate.ListOfTypes where

import Data.Kind (type (*))
import Data.Proxy

import Data.Promoted.Nat
import Data.Promoted.List

infixr 5 :&:
data LoT k where
  LoT0   ::                LoT (*)
  (:&:)  :: k -> LoT ks -> LoT (k -> ks)

data SLoT k (tys :: LoT k) where
  SLoT0 ::               SLoT (*)       LoT0
  SLoTA :: SLoT ks ts -> SLoT (k -> ks) (t :&: ts)

class SSLoT k (tys :: LoT k) | tys -> k where
  sslot :: SLoT k tys
instance SSLoT (*) LoT0 where
  sslot = SLoT0
instance SSLoT ks ts => SSLoT (k -> ks) (t :&: ts) where
  sslot = SLoTA sslot

data ApplyT k (f :: k) (tys :: LoT k) where
  A0 :: { unA0  :: f }
      -> ApplyT (*) f LoT0
  A# :: { unA# :: ApplyT ks (f t) ts }
     -> ApplyT (k -> ks) f (t :&: ts)

type family Apply k (f :: k) (tys :: LoT k) :: (*) where
  Apply (*)       f LoT0       = f
  Apply (k -> ks) f (t :&: ts) = Apply ks (f t) ts

unravel :: ApplyT k f tys -> Apply k f tys
unravel (A0 x) = x
unravel (A# x) = unravel x

ravel  :: SSLoT k tys => Apply k f tys -> ApplyT k f tys
ravel = go sslot
  where
    go  :: SLoT k tys -> Apply k f tys -> ApplyT k f tys
    go SLoT0      x = A0 x
    go (SLoTA ts) x = A# (go ts x)

data El k (fs :: [k]) (n :: Nat) (tys :: LoT k) where
  El :: SNat n -> ApplyT k (Lkp n fs) tys -> El k fs n tys

type El1 k (f :: k) (tys :: LoT k) = El k '[f] Z tys

unravelEl :: El k fs n tys -> Apply k (Lkp n fs) tys
unravelEl (El _ x) = unravel x

ravelEl :: forall k fs n tys.
           (SSLoT k tys, SSNat n)
        => Apply k (Lkp n fs) tys -> El k fs n tys
ravelEl x = El ssnat (ravel x)