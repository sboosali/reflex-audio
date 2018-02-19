
{-# OPTIONS_GHC -fno-warn-missing-signatures #-} -- to test inference

{-
   TestWaveforms.hs (adapted from test_waveforms.c in freealut)
   Copyright (c) Sven Panne 2005-2016
   This file is part of the ALUT package & distributed under a BSD-style license.
   See the file LICENSE.
-}

{-| This module provides an example program. 
Please read the source too <https://hackage.haskell.org/package/reflex-audio/docs/src/Reflex-Audio-Example.html (direct Hackage link)>. 

Being a @library@ module, it's typechecked with the package, 
and thus should always build.

Only public interfaces are imported (i.e. no @.Internal@s),
and there are minimal other dependencies. 

'main' is executed by the @reflex-audio-example@ executable. 

-}

module Reflex.Audio.Example where

import Sound.ALUT
-- import Reflex.Audio()
-- import System.Environment

import Prelude

----------------------------------------

main :: IO ()
main = mainWith []

----------------------------------------

-- This program plays a 440Hz tone using a variety of waveforms.
mainWith :: [String] -> IO ()
mainWith _ = do
   withProgNameAndArgs runALUT $ \_progName _args ->
      mapM_ playTone [ Sine, Square, Sawtooth, (const (const WhiteNoise)), Impulse ]
  
playTone :: (Frequency -> Phase -> Duration -> SoundDataSource a) -> IO ()
playTone soundDataSource = do
   buf <- createBuffer (soundDataSource 440 0 1)
   source <- genObjectName
   buffer source $= Just buf
   play [source]
   sleep 1

{-

main :: IO ()
main = do
 arguments <- getArgs >>= \case
  [s] -> return (s)
  _ -> return ("")
 mainWith arguments

mainWith s = do
 putStrLn s
 putStrLn "(Reflex.Audio.Example...)"

-}
