
{-| The core definitions. 

The primitive behaviors are continuous functions.



-}
module Reflex.Audio.Core where
import Reflex.Audio.Extra
import Reflex.Audio.Types

import Reflex

--import Prelude.Spiros hiding (sine)

----------------------------------------

sine :: (Reflex t) => Behavior t (Signal Frequency) -> Behavior t (Signal Energy)
sine bFrequency = bEnergy
  where
  bEnergy = bFrequency & _
  

{-NOTES



(\(Time t, Frequency w) v -> let d = t * w in V.write v d)

V.foldl' :: ((Time, Frequency) -> Vector Amplitude -> Vector Amplitude) -> Vector (Time, Frequency) -> Vector Amplitude


foldl' :: (a -> b -> a) -> a -> Vector b -> a




-}