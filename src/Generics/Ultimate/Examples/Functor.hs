{-# language TypeOperators         #-}
{-# language DataKinds             #-}
{-# language ExplicitNamespaces    #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances     #-}
{-# language TypeFamilies          #-}
{-# language MagicHash             #-}
{-# language TypeApplications      #-}
{-# language TypeInType            #-}
{-# language ScopedTypeVariables   #-}
{-# language FlexibleContexts      #-}
module Generics.Ultimate.Examples.Functor where

import Generics.Ultimate

class FunctorField (t :: Atom (* -> *) (*)) where
  gfmapF :: (a -> b)
         -> NA (* -> *) (a :&: LoT0) (Value t)
         -> NA (* -> *) (b :&: LoT0) (Value t)

instance FunctorField V0 where
  gfmapF f (V x) = V (f x)
instance forall f a x. (Functor (f a), FunctorField x)
         => FunctorField (Kon f :@: Kon a :@: x) where
  gfmapF f (V x) = V (fmap (unV . gfmapF f . V @_ @_ @x) x)
instance forall f x. (Functor f, FunctorField x)
         => FunctorField (Kon f :@: x) where
  gfmapF f (V x) = V (fmap (unV . gfmapF f . V @_ @_ @x) x)
instance FunctorField (Kon t) where
  gfmapF f (V x) = V x

gfmap :: forall f a b.
         (Generic f, AllValuesD FunctorField (Code f))
      => (a -> b) -> f a -> f b
gfmap f = unravel . to . goS . from . ravel
  where
    goS :: AllValuesD FunctorField xs
        => NS (NP (NA (* -> *) (a :&: LoT0))) xs
        -> NS (NP (NA (* -> *) (b :&: LoT0))) xs
    goS (Here  x) = Here  (goP x)
    goS (There x) = There (goS x)

    goP :: AllValuesC FunctorField xs
        => NP (NA (* -> *) (a :&: LoT0)) xs
        -> NP (NA (* -> *) (b :&: LoT0)) xs
    goP Nil         = Nil
    goP (V x :* xs) = gfmapF f (V x) :* goP xs
  
