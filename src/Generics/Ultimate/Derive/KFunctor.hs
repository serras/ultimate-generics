{-# language DataKinds             #-}
{-# language GADTs                 #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeOperators         #-}
{-# language TypeApplications      #-}
{-# language MagicHash             #-}
{-# language ScopedTypeVariables   #-}
{-# language KindSignatures        #-}
{-# language TypeInType            #-}
{-# language DefaultSignatures     #-}
{-# language FlexibleInstances     #-}
{-# language TypeFamilies          #-}
module Generics.Ultimate.Derive.KFunctor where

import Generics.Ultimate

data Mappings (as :: LoT dk) (bs :: LoT dk) where
  MNil  :: Mappings LoT0 LoT0
  MCons :: (a -> b) -> Mappings as bs -> Mappings (a :&: as) (b :&: bs)

class KFunctor (f :: k) where
  kmap :: SSLoT k bs => Mappings as bs -> ApplyT k f as -> ApplyT k f bs

  default kmap :: (Generic f, SSLoT k bs,
                   AllValuesD KFunctorField (Code f), SatisfyConstrD k bs (Code f))
               => Mappings as bs -> ApplyT k f as -> ApplyT k f bs
  kmap fs = to . gkmap (Proxy :: Proxy f) fs . from

fmapDefault :: (Generic f, KFunctor f)
             => (a -> b) -> f a -> f b
fmapDefault f = unravel . kmap (MCons f MNil) . ravel

bimapDefault :: (Generic f, KFunctor f)
             => (a -> b) -> (c -> d) -> f a c -> f b d
bimapDefault f g = unravel . kmap (MCons f (MCons g MNil)) . ravel

{-
class BuildMapper k where
  type Mapper k (as :: LoT k) (bs :: LoT k) t
  mapper :: (Mappings as bs -> t)
         -> Mapper k as bs t

instance BuildMapper (*) where
  type Mapper (*) LoT0 LoT0 t = t 
  mapper f = f MNil

instance BuildMapper l => BuildMapper (k -> l) where
  type Mapper (k -> l) (a :&: as) (b :&: bs) t = (a -> b) -> Mapper l as bs t
-}

-- Some automatically derived instances

instance KFunctor []
instance KFunctor Maybe
instance KFunctor Either

-- Implementation of generic kmap

gkmap :: forall k (f :: k) (as :: LoT k) (bs :: LoT k)
       . (Generic f, AllValuesD KFunctorField (Code f), SatisfyConstrD k bs (Code f))
      => Proxy f -> Mappings as bs -> SOP k (Code f) as -> SOP k (Code f) bs
gkmap _ f = goS
  where
    goS :: (AllValuesD KFunctorField xs, SatisfyConstrD k bs xs)
        => NS (NP (NA k as)) xs -> NS (NP (NA k bs)) xs
    goS (Here  x) = Here  (goP x)
    goS (There x) = There (goS x)

    goP :: (AllValuesC KFunctorField xs, SatisfyConstrC k bs xs)
        => NP (NA k as) xs -> NP (NA k bs) xs
    goP Nil         = Nil
    goP (V x :* xs) = kmapf f (V x) :* goP xs
    goP (C   :* xs) = C :* goP xs

class KFunctorField (t :: Atom dk (*)) where
  kmapf :: Mappings as bs -> NA dk as (Value t) -> NA dk bs (Value t)

data STyVar k (t :: TyVar k (*)) where
  SVZ :: STyVar (* -> k) VZ
  SVS :: STyVar k v -> STyVar (* -> k) (VS v)

class SForTyVar k (t :: TyVar k (*)) where
  styvar :: STyVar k t
instance SForTyVar (* -> k) VZ where
  styvar = SVZ
instance SForTyVar k v => SForTyVar (* -> k) (VS v) where
  styvar = SVS styvar

instance forall k (v :: TyVar k (*)). SForTyVar k v => KFunctorField (Var v) where
  kmapf f (V x) = V (go (styvar @k @v) f x)
    where go :: forall k (as :: LoT k) (bs :: LoT k) (v :: TyVar k (*))
              . STyVar k v -> Mappings as bs -> Ty k as (Var v) -> Ty k bs (Var v)
          go SVZ      (MCons g _)  x = g x
          go (SVS v') (MCons _ f') x = go v' f' x

instance KFunctorField (Kon t) where
  kmapf f (V x) = V x

instance forall f x. (KFunctorHead f, KFunctorField x) => KFunctorField (f :@: x) where
  kmapf f (V x) = V $ unA0 $ unA#
                $ kmaph (Proxy :: Proxy f) f (MCons (unV . kmapf f . V @_ @_ @x) MNil)
                $ A# $ A0 x

class KFunctorHead (t :: Atom dk k) where
  kmaph :: SSLoT k ts => Proxy t
        -> Mappings as bs -> Mappings rs ts 
        -> ApplyT k (Ty dk as t) rs -> ApplyT k (Ty dk bs t) ts

instance forall f x. (KFunctorHead f, KFunctorField x) => KFunctorHead (f :@: x) where
  kmaph _ f r x = unA#
                $ kmaph (Proxy :: Proxy f) f (MCons (unV . kmapf f . V @_ @_ @x) r)
                $ A# x

instance forall f. (KFunctor f) => KFunctorHead (Kon f) where
  kmaph _ _ r x = kmap r x