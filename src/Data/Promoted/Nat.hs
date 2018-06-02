{-# language GADTs                 #-}
{-# language DataKinds             #-}
{-# language KindSignatures        #-}
{-# language TypeInType            #-}
{-# language TypeFamilies          #-}
{-# language ScopedTypeVariables   #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}
{-# language UndecidableInstances  #-}
module Data.Promoted.Nat where

import Data.Proxy

data Nat = Z | S Nat

data SNat (n :: Nat) where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

class SSNat (n :: Nat) where
  ssnat :: SNat n
instance SSNat Z where
  ssnat = SZ
instance SSNat n => SSNat (S n) where
  ssnat = SS ssnat

class SSNat' n (s :: SNat n) | s -> n, n -> s where
  ssnat' :: SNat n
instance SSNat' Z SZ where
  ssnat' = SZ
instance SSNat' n s => SSNat' (S n) (SS s) where
  ssnat' = SS ssnat'

type N0 = SZ
type N1 = SS N0
type N2 = SS N1
type N3 = SS N2
type N4 = SS N3
type N5 = SS N4
type N6 = SS N5
type N7 = SS N6
type N8 = SS N7
type N9 = SS N8