{-# language ExplicitNamespaces #-}
module Generics.Ultimate (
  module Export
, type (*)
, Constraint
) where

import Data.Kind (type (*), Constraint)
import Data.Proxy as Export
import Generics.Ultimate.Code as Export
import Generics.Ultimate.ListOfTypes as Export
import Generics.Ultimate.Interpretation as Export
import Generics.Ultimate.Class as Export
import Generics.Ultimate.Constraints as Export
import Generics.Ultimate.Instances as Export
