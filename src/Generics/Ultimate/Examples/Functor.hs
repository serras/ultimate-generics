{-# language TypeOperators         #-}
{-# language DataKinds             #-}
{-# language ExplicitNamespaces    #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances     #-}
{-# language TypeFamilies          #-}
{-# language MagicHash             #-}
{-# language TypeApplications      #-}
{-# language TypeInType            #-}
{-# language RankNTypes            #-}
{-# language ScopedTypeVariables   #-}
{-# language FlexibleContexts      #-}
module Generics.Ultimate.Examples.Functor where

import Generics.Ultimate

class FunctorField r (t :: Atom (* -> *) (* -> *) (*)) where
  gfmapF :: (forall x y. (x -> y) -> r x -> r y)
         -> (a -> b)
         -> NA (* -> *) (* -> *) '[r] (a :&: LoT0) (Explicit t)
         -> NA (* -> *) (* -> *) '[r] (b :&: LoT0) (Explicit t)

instance FunctorField r V0 where
  gfmapF _ f (E x) = E (f x)
instance (FunctorField r x)
         => FunctorField r (R0 :@: x) where
  gfmapF r f (E x) = E (r (unE . gfmapF r f . E @_ @_ @x) x)
instance (Functor (f a), FunctorField r x)
         => FunctorField r (Kon f :@: Kon a :@: x) where
  gfmapF r f (E x) = E (fmap (unE . gfmapF r f . E @_ @_ @x) x)
instance (Functor f, FunctorField r x)
         => FunctorField r (Kon f :@: x) where
  gfmapF r f (E x) = E (fmap (unE . gfmapF r f . E @_ @_ @x) x)
instance FunctorField r (Kon t) where
  gfmapF _ f (E x) = E x

gfmap :: forall f a b code.
         (Generic f, AllD (FunctorField f) (Code f))
      => (a -> b) -> f a -> f b
gfmap f = unravel . to . goS . from . ravel
  where
    goS :: AllD (FunctorField f) xs
        => NS (NB (* -> *) (* -> *) '[f] (a :&: LoT0)) xs
        -> NS (NB (* -> *) (* -> *) '[f] (b :&: LoT0)) xs
    goS (Here x)  = Here  (goB x)
    goS (There x) = There (goS x)

    goB :: AllB (FunctorField f) xs
        => NB (* -> *) (* -> *) '[f] (a :&: LoT0) xs
        -> NB (* -> *) (* -> *) '[f] (b :&: LoT0) xs
    goB (Cr x) = Cr (goP x)

    goP :: AllE (FunctorField f) xs
        => NP (NA (* -> *) (* -> *) '[f] (a :&: LoT0)) xs
        -> NP (NA (* -> *) (* -> *) '[f] (b :&: LoT0)) xs
    goP Nil         = Nil
    goP (E x :* xs) = gfmapF gfmap f (E x) :* goP xs
  
