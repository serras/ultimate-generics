{-# language DataKinds            #-}
{-# language ConstraintKinds      #-}
{-# language ExplicitNamespaces   #-}
{-# language TypeOperators        #-}
{-# language GADTs                #-}
{-# language TypeFamilies         #-}
{-# language PolyKinds            #-}
{-# language KindSignatures       #-}
{-# language TypeInType           #-}
{-# language UndecidableInstances #-}
module Generics.Ultimate.Code where

import Data.Kind (type (*), Constraint)
import GHC.TypeLits (TypeError, ErrorMessage(..))

import Data.Promoted.Nat

type Kind = (*)

data Atom (rk :: Kind) (dk :: Kind) k where
  Var   :: SNat n
        -> Atom rk dk (Pos n dk)
  Rec   :: SNat n
        -> Atom rk dk rk
  Kon   :: k
        -> Atom rk dk k
  (:@:) :: Atom rk dk (k1 -> k2)
        -> Atom rk dk k1
        -> Atom rk dk k2

type family Pos (n :: Nat) (dk :: Kind) :: Kind where
  Pos n      (*)       = TypeError (Text "Not found")
  Pos Z      (x -> xs) = x
  Pos (S n)  (x -> xs) = Pos n xs

data Field (rk :: Kind) (dk :: Kind) where
  Explicit  :: Atom rk dk (*)
            -> Field rk dk
  Implicit  :: Atom rk dk Constraint
            -> Field rk dk

type Fields rk dk = [Field rk dk]

data SKind (k :: Kind) = KK

data Branch (rk :: Kind) (dk :: Kind) where
  Exists  :: SKind k -> Branch rk (k -> dk)
          -> Branch rk dk
  Constr  :: Fields rk dk
          -> Branch rk dk

type DataType dk = [Branch dk dk]
type Family   dk = [DataType dk]

type V0 = Var N0
type V1 = Var N1
type V2 = Var N2
type V3 = Var N3
type V4 = Var N4
type V5 = Var N5
type V6 = Var N6
type V7 = Var N7
type V8 = Var N8
type V9 = Var N9

type R0 = Rec N0
type R1 = Rec N1
type R2 = Rec N2
type R3 = Rec N3
type R4 = Rec N4
type R5 = Rec N5
type R6 = Rec N6
type R7 = Rec N7
type R8 = Rec N8
type R9 = Rec N9