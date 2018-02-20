{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--{-# LANGUAGE TypeApplications #-}

{-| The core types.



This module mostly defines types 
(i.e. @data@, @newtype@, @type@, @class@, @instance@) 
and whatever values are necessary for instances.



-}
module Reflex.Audio.Types where

-- import Reflex.Audio.Extra()
-- import Reflex.Audio.Types()

-- import Prelude.Spiros

import           Data.Vector (Vector)
import qualified Data.Vector as V
--import Data.Vector.Unboxed

import Sound.OpenAL.AL

import Prelude.Spiros
import Data.Kind (Constraint)
import GHC.TypeLits
import Foreign.Storable (Storable)
import Text.Printf (PrintfArg)

----------------------------------------

{-|

-}
data Signal a
  = ConstantSignal a
  | BoundedSignal a (Vector a)
  deriving (Functor)

newtype Frequency = Frequency { getFrequency :: Double }
 deriving (Num,Fractional,Real,RealFrac,Floating,RealFloat,Eq,Ord,Show,Read ,Storable,PrintfArg)

type Energy = Double

-- newtype Time = Time { getTime :: Double }
--  deriving (Num,Fractional,Real,RealFrac,Floating,RealFloat,Eq,Ord,Show,Read ,Storable,PrintfArg)

-- newtype Amplitude = Amplitude { getAmplitude :: Double }
--  deriving (Num,Fractional,Real,RealFrac,Floating,RealFloat,Eq,Ord,Show,Read ,Storable,PrintfArg)

{-

data Signal t a
  = ConstantSignal a
  | BoundedSignal (t -> a) (a -> Vector a)
  deriving (Functor)

-}

----------------------------------------

{-|

-}
newtype ALSignal (n :: Nat) (u :: Format) = ALSignal {
  fromALSignal :: Vector (FormatType u)
 }
-- } deriving (Show)

instance (Show (FormatType u)) => Show (ALSignal n u) where
  show (ALSignal v) = show v

{-|

-}
type family FormatType (u :: Format) :: * where
  FormatType Mono8    = ALubyte
  FormatType Mono16   = ALshort

{-|

-}
instance (Num (FormatType u), KnownNat n) => Num (ALSignal n u) where
  (+)         = zipALSignal (+)
  (-)         = zipALSignal (-)
  (*)         = zipALSignal (*)
  negate      = mapALSignal (negate)
  abs         = mapALSignal (abs)
  signum      = mapALSignal (signum)
  fromInteger = \i -> constantALSignal (fromInteger i)

{-|

-}
constantALSignal
  :: forall n u.
     (KnownNat n) 
  => FormatType u
  -> ALSignal n u
constantALSignal i = ALSignal (V.replicate k i)
  where
  k = fromInteger $ natVal (Proxy :: Proxy n)

{-|

-}
mapALSignal
  :: (FormatType u -> FormatType u)
  -> (ALSignal   n u -> ALSignal   n u)
mapALSignal f (ALSignal v) = ALSignal (V.map f v)

{-|

-}
zipALSignal
  :: (FormatType u -> FormatType u -> FormatType u)
  -> (ALSignal   n u -> ALSignal   n u -> ALSignal   n u)
zipALSignal f (ALSignal v) (ALSignal u) = ALSignal (V.zipWith f u v)

{-|

-}
type family IsALSignal (c :: * -> Constraint) (u :: Format) :: Constraint where
  IsALSignal c u = (c (FormatType u))

----------------------------------------

-- map :: (Unbox a, Unbox b) => (a -> b) -> Vector a -> Vector b

{-

{-|

An unboxed vector

-}
newtype ALSignal (u :: Format) = Signal {
  fromSignal :: Vector (FormatType u)
 }

type family FormatType u :: *
  FormatType Mono8    = 
  FormatType Mono16   =  
  FormatType Stereo8  =  
  FormatType Stereo16 =  


data instance MVector s (a, b)
    = MV_2 {-# UNPACK #-} !Int !(MVector s a)
                               !(MVector s b)

-}

{-

type ALshort = CShort Source

Signed 16-bit 2's complement integer



Format
=
Mono8	 
Mono16	 
Stereo8	 
Stereo16	 



-}


{-|

-- `Floating` is: Trigonometric and hyperbolic functions and related functions.



-}
-- data Continuous a
--  = Constant a
--  | Sin    a
 
{-
-- | Exp    a
 -- | Log    a

 -- | Cos    a
 -- | Asin   a
 -- | Acos   a
 -- | Atan   a
 -- | Sinh   a
 -- | Cosh   a
 -- | Asinh  a
 -- | Acosh  a
 -- | Atanh  a
-}


-- instance Num a => Num (Continuous a) where

-- instance Fractional a => Floating (Continuous a) where 
--   pi = Continuous pi

--   log
--   sqrt
  
--   exp, log, sin, cos, asin, acos, atan, sinh, cosh, asinh, acosh, atanh

-- pi :: a Source#

-- exp, :: a -> a Source#

-- (**), logBase :: a -> a -> a infixr 8 Source#

-- sin, cos, tan :: a -> a Source#

-- asin, acos, atan :: a -> a Source#

-- sinh, cosh, tanh :: a -> a Source#

-- asinh, acosh, atanh :: a -> a Source#

