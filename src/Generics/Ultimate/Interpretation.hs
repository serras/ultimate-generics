{-# language DataKinds          #-}
{-# language ConstraintKinds    #-}
{-# language ExplicitNamespaces #-}
{-# language TypeOperators      #-}
{-# language GADTs              #-}
{-# language TypeFamilies       #-}
{-# language PolyKinds          #-}
{-# language KindSignatures     #-}
{-# language TypeInType         #-}
{-# language ExplicitForAll     #-}
module Generics.Ultimate.Interpretation where

import Data.Kind (type (*), type Type, Constraint)

import Data.Promoted.Nat
import Data.Promoted.List
import Generics.Ultimate.Code
import Generics.Ultimate.ListOfTypes

type family Ty (rk :: Kind)   (dk  :: Kind)
               (rs :: [reck]) (tys :: LoT datak)
               (t :: Atom reck datak k) :: k where
  Ty rk (k1 -> ks) rs (t0 :&: ts) V0 = t0
  Ty rk (k1 -> k2 -> ks) rs (t0 :&: t1 :&: ts) V1 = t1
  Ty rk (k1 -> k2 -> k3 -> ks) rs (t0 :&: t1 :&: t2 :&: ts) V2  = t2
  Ty rk (k1 -> k2 -> k3 -> k4 -> ks) rs (t0 :&: t1 :&: t2 :&: t3 :&: ts) V3  = t3
  Ty rk (k1 -> k2 -> k3 -> k4 -> k5 -> ks) rs (t0 :&: t1 :&: t2 :&: t3 :&: t4 :&: ts) V4  = t4

  Ty rk dk (r0 ': rs) tys R0 = r0
  Ty rk dk (r0 ': r1 ': rs) tys R1 = r1
  Ty rk dk (r0 ': r1 ': r2 ': rs) tys R2 = r2
  Ty rk dk (r0 ': r1 ': r2 ': r3 ': rs) tys R3 = r3
  Ty rk dk (r0 ': r1 ': r2 ': r3 ': r4 ': rs) tys R4 = r4

  Ty rk dk rs tys (Kon t)   = t
  Ty rk dk rs tys (f :@: x) = (Ty rk dk rs tys f) (Ty rk dk rs tys x)

data NA (rk :: Kind) (dk :: Kind) (rs :: [rk]) (tys :: LoT dk) :: Field rk dk -> * where
  E :: forall rk dk t rs tys.
       Ty rk dk rs tys t -> NA rk dk rs tys (Explicit t)
  I :: forall rk dk t rs tys. 
       Ty rk dk rs tys t => NA rk dk rs tys (Implicit t)

unE :: forall rk dk t rs tys.
       NA rk dk rs tys (Explicit t) -> Ty rk dk rs tys t
unE (E x) = x
      
infixr 5 :*
data NP :: (k -> *) -> [k] -> * where
  Nil  ::                    NP f '[]
  (:*) :: f x -> NP f xs ->  NP f (x ': xs)

data NB (rk :: Kind) (dk :: Kind) (rs :: [rk]) (tys :: LoT dk) :: Branch rk dk -> * where
  Ex  :: forall k (t :: k) (p :: SKind k) rk dk rs tys c .
         NB rk (k -> dk) rs (t :&: tys) c -> NB rk dk rs tys (Exists p c)
  Cr  :: NP (NA rk dk rs tys) fs          -> NB rk dk rs tys (Constr fs)

data NS :: (k -> *) -> [k] -> * where
  Here  :: f k      -> NS f (k ': ks)
  There :: NS f ks  -> NS f (k ': ks)

data SOP dk (fam :: Family dk) (n :: Nat) (rs :: [dk]) (tys :: LoT dk) where
  SOP :: SNat n -> NS (NB dk dk rs tys) (Lkp n fam) -> SOP dk fam n rs tys
