
{-# OPTIONS_GHC -fno-warn-missing-signatures #-} -- to test inference

{-| This module provides an example program. 
Please read the source too <https://hackage.haskell.org/package/reflex-audio/docs/src/Reflex-Audio-Example.html (direct Hackage link)>. 

Being a @library@ module, it's typechecked with the package, 
and thus should always build.

Only public interfaces are imported (i.e. no @.Internal@s),
and there are minimal other dependencies. 

'main' is executed by the @reflex-audio-example@ executable. 

-}
module Reflex.Audio.Example where

import Reflex.Audio()

import System.Environment

import Prelude

{-|

Running:

@
cabal build && cabal run example-reflex-audio
@

@
stack build && stack exec -- example-reflex-audio
@

-}
main :: IO ()
main = do
 arguments <- getArgs >>= \case
  [s] -> return (s)
  _ -> return ("")
 mainWith arguments

mainWith s = do
 putStrLn s
 putStrLn "(Reflex.Audio.Example...)"

