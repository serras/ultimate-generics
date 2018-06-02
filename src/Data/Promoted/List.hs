{-# language DataKinds             #-}
{-# language TypeFamilies          #-}
{-# language TypeOperators         #-}
{-# language PolyKinds             #-}
{-# language UndecidableInstances  #-}
{-# language GADTs                 #-}
{-# language FlexibleInstances     #-}
{-# language MultiParamTypeClasses #-}
module Data.Promoted.List where

import GHC.TypeLits (TypeError, ErrorMessage(..))

import Data.Promoted.Nat

type family Lkp (n :: Nat) (xs :: [k]) :: k where
  Lkp Z     '[]       = TypeError (Text "Index out of bounds")
  Lkp Z     (x ': xs) = x
  Lkp (S n) (x ': xs) = Lkp n xs

type family Ix (x :: k) (xs :: [k]) :: Nat where 
  Ix x '[] = TypeError (Text "Cannot find element")
  Ix x (x ': ys) = Z
  Ix x (y ': ys) = S (Ix x ys)

data SList (xs :: [k]) where
  SNil  :: SList '[]
  SCons :: SList xs -> SList (x ': xs)