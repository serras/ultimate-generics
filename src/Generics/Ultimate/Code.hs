{-# language DataKinds            #-}
{-# language ConstraintKinds      #-}
{-# language ExplicitNamespaces   #-}
{-# language TypeOperators        #-}
{-# language GADTs                #-}
{-# language TypeFamilies         #-}
{-# language KindSignatures       #-}
{-# language TypeInType           #-}
module Generics.Ultimate.Code where

import Data.Kind (type (*), Constraint)
import GHC.TypeLits (TypeError, ErrorMessage(..))

type Kind = (*)

data TyVar (dk :: Kind) k where
  VZ :: TyVar (k -> ls) k
  VS :: TyVar ls k
     -> TyVar (l -> ls) k

data Atom (dk :: Kind) k where
  Var   :: TyVar dk k
        -> Atom dk k
  Kon   :: k
        -> Atom dk k
  (:@:) :: Atom dk (k1 -> k2)
        -> Atom dk k1
        -> Atom dk k2

data Field (dk :: Kind) where
  Value  :: Atom dk (*)
         -> Field dk
  Constr :: Atom dk Constraint
         -> Field dk

type Constructor dk = [Field dk]
type DataType    dk = [Constructor dk]

type V0 = Var VZ
type V1 = Var (VS VZ)
type V2 = Var (VS (VS VZ))
type V3 = Var (VS (VS (VS VZ)))
type V4 = Var (VS (VS (VS (VS VZ))))
type V5 = Var (VS (VS (VS (VS (VS VZ)))))
type V6 = Var (VS (VS (VS (VS (VS (VS VZ))))))
type V7 = Var (VS (VS (VS (VS (VS (VS (VS VZ)))))))
type V8 = Var (VS (VS (VS (VS (VS (VS (VS (VS VZ))))))))
type V9 = Var (VS (VS (VS (VS (VS (VS (VS (VS (VS VZ)))))))))