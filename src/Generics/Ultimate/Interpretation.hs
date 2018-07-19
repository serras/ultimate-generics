{-# language DataKinds           #-}
{-# language ConstraintKinds     #-}
{-# language ExplicitNamespaces  #-}
{-# language TypeOperators       #-}
{-# language GADTs               #-}
{-# language TypeFamilies        #-}
{-# language PolyKinds           #-}
{-# language KindSignatures      #-}
{-# language TypeInType          #-}
{-# language ScopedTypeVariables #-}
module Generics.Ultimate.Interpretation where

import Data.Kind (type (*), type Type, Constraint)

import Generics.Ultimate.Code
import Generics.Ultimate.ListOfTypes

type family Ty (dk  :: Kind) (tys :: LoT dk) (t :: Atom dk k) :: k where
  Ty (k -> ls) (t :&: ts) (Var VZ)     = t
  Ty (k -> ls) (t :&: ts) (Var (VS v)) = Ty ls ts (Var v)
  Ty dk tys (Kon t)   = t
  Ty dk tys (f :@: x) = (Ty dk tys f) (Ty dk tys x)

data NA (dk :: Kind) (tys :: LoT dk) :: Field dk -> * where
  V :: { unV :: Ty dk tys t } -> NA dk tys (Value  t)
  C ::          Ty dk tys t   => NA dk tys (Constr t)
      
infixr 5 :*
data NP :: (k -> *) -> [k] -> * where
  Nil  ::                    NP f '[]
  (:*) :: f x -> NP f xs ->  NP f (x ': xs)

data NS :: (k -> *) -> [k] -> * where
  Here  :: f k      -> NS f (k ': ks)
  There :: NS f ks  -> NS f (k ': ks)

type SOP dk (dt :: DataType dk) (tys :: LoT dk) = NS (NP (NA dk tys)) dt