
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

import Reflex.Audio.Extra

import Sound.ALUT

-- import System.Environment

--import Sound.OpenAL.AL.StringQueries

----------------------------------------

main :: IO ()
main = do
  mainInfo
  -- mainWith []

----------------------------------------

mainInfo = runALUT_ $ do
  putStrLn "\n[version]"
  alVersion >>= print

  vendor   <- get alVendor
  renderer <- get alRenderer

  putStrLn "\n[vendor]"
  print vendor
  
  putStrLn "\n[renderer]"
  print renderer

  putStrLn "\n[extensions]"
  alExtensions >>= traverse_ print

  {- e.g.

    [version]
    "1.1 ALSOFT 1.18.2"
    
    [vendor]
    "OpenAL Community"
    
    [renderer]
    "OpenAL Soft"
    
    [extensions]
    "AL_EXT_ALAW"
    "AL_EXT_BFORMAT"
    "AL_EXT_DOUBLE"
    "AL_EXT_EXPONENT_DISTANCE"
    "AL_EXT_FLOAT32"
    "AL_EXT_IMA4"
    "AL_EXT_LINEAR_DISTANCE"
    "AL_EXT_MCFORMATS"
    "AL_EXT_MULAW"
    "AL_EXT_MULAW_BFORMAT"
    "AL_EXT_MULAW_MCFORMATS"
    "AL_EXT_OFFSET"
    "AL_EXT_source_distance_model"
    "AL_EXT_SOURCE_RADIUS"
    "AL_EXT_STEREO_ANGLES"
    "AL_LOKI_quadriphonic"
    "AL_SOFT_block_alignment"
    "AL_SOFT_deferred_updates"
    "AL_SOFT_direct_channels"
    "AL_SOFT_gain_clamp_ex"
    "AL_SOFT_loop_points"
    "AL_SOFT_MSADPCM"
    "AL_SOFT_source_latency"
    "AL_SOFT_source_length"
    "AL_SOFT_source_resampler"
    "AL_SOFT_source_spatialize"
    
  -}
  

runALUT_ action = withProgNameAndArgs runALUT $ 
  \_executableName _arguments -> do
    action

runALUTWith actionWith = withProgNameAndArgs runALUT $ do
  \_executableName arguments -> do
    actionWith arguments

----------------------------------------

-- This program plays a 440Hz tone using a variety of waveforms.
mainWith :: [String] -> IO ()
mainWith _ = runALUTWith $ \arguments ->
      mapM_ playTone [ Sine, Square, Sawtooth, (const (const WhiteNoise)), Impulse ]

playTone
  :: (Frequency -> Phase -> Duration -> SoundDataSource a)
  -> IO ()
playTone toSource = do
   buf <- createBuffer (toSource 440 0 1)
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

----------------------------------------

{-NOTES

GLUT...


type Phase = Float Source

type Duration = Float Source

data SoundDataSource a
=
File FilePath	 
FileImage (MemoryRegion a)	 
HelloWorld	 
Sine Frequency Phase Duration	 
Square Frequency Phase Duration	 
Sawtooth Frequency Phase Duration	 
Impulse Frequency Phase Duration	 
WhiteNoise Duration












OPENAL...



module Sound.OpenAL.AL

OpenAL is concerned with rendering audio into an output buffer and collecting audio data from an input buffer.
OpenAL's primary use is assumed to be for spatialized audio. There is no support for MIDI.

OpenAL has three fundamental primitives or objects: Buffers, Sources, and a single listener (see Sound.OpenAL.AL.Listener)

- Sources
Sources store locations, directions, and other attributes of an object in 3D space and have a buffer associated with them for playback. When the program wants to play a sound, it controls execution through a source object. Sources are processed independently from each other.

- Buffers
Buffers store compressed or uncompressed audio data. It is common to initialize a large set of buffers when the program first starts (or at non-critical times during execution, between levels in a game, for instance). Buffers are referred to by sources. Data (audio sample data) is associated with buffers.

- Listener
There is only one listener (per audio context). The listener attributes are similar to source attributes, but are used to represent where the user is hearing the audio from. The influence of all the sources from the perspective of the listener is mixed and played for the user.

For OpenAL, initialization normally consists of creating a context, creating the initial set of buffers, loading the buffers with sample data, creating sources, attaching buffers to sources, setting locations and directions for the listener and sources, and setting the initial values for state global to OpenAL.


Time and Frequency
By default, OpenAL uses seconds and Hertz as units for time and frequency, respectively. A float or integral value of one for a variable that specifies quantities like duration, latency, delay, or any other parameter measured as time, specifies 1 second. For frequency, the basic unit is 1/second, or Hertz. In other words, sample frequencies and frequency cut-offs or filter parameters specifying frequencies are expressed in units of Hertz.

Space and Distance
OpenAL does not define the units of measurement for distances. The application is free to use meters, inches, or parsecs. OpenAL provides means for simulating the natural attenuation of sound according to distance, and to exaggerate or reduce this effect. However, the resulting effects do not depend on the distance unit used by the application to express source and listener coordinates. OpenAL calculations are scale invariant. The specification assumes Euclidean calculation of distances, and mandates that if two sources are sorted with respect to the Euclidean metric, the distance calculation used by the implementation has to preserve that order.




Sound.OpenAL.AL.StringQueries

alVendor :: GettableStateVar String 
alRenderer :: GettableStateVar String







OpenAL.Buffer

data Buffer
The abstract buffer type.








data MemoryRegion a
=
MemoryRegion (Ptr a) ALsizei





data BufferData a
=
BufferData (MemoryRegion a) Format Frequency

bufferData :: Buffer -> StateVar (BufferData a)
bufferData :: Buffer -> StateVar (BufferData a)

Buffers containing audio data with more than one channel will be played without 3D spatialization features, these formats are normally used for background music. Applications should always check for an error condition after attempting to specify buffer data in case an implementation has to generate an ALOutOfMemory or a conversion related ALInvalidValue error. The application is free to reuse the memory specified by the data pointer once bufferData is set. The implementation has to dereference, e.g. copy, the data while accessing bufferData execution.






-- | Valid sound formats.
data Format =
     Mono8
   | Mono16
   | Stereo8
   | Stereo16






data ALErrorCategory =
     ALInvalidEnum
   | ALInvalidValue
   | ALInvalidOperation
   | ALInvalidName
   | ALOutOfMemory



-- | The application can choose to specify certain attributes for a context at
-- context-creation time. Attributes not specified explicitly are set to
-- implementation dependent defaults.

data ContextAttribute =
     Frequency Frequency   -- ^ Frequency for mixing output buffer, in units of
                           --   Hz
   | Refresh Frequency     -- ^ Refresh intervals, in units of Hz
   | Sync Bool             -- ^ Flag, indicating a synchronous context
   | MonoSources Int       -- ^ A hint indicating how many sources should be
                           --   capable of supporting mono data
   | StereoSources Int     -- ^ A hint indicating how many sources


createContext :: MonadIO m => Device -> [ContextAttribute] -> m (Maybe Context)


allAttributes :: Device -> GettableStateVar [ContextAttribute] 
Contains the attribute list for the current context of the specified device.







data Device 
The abstract device type.

ALC introduces the notion of a device. A device can be, depending on the implementation, a hardware device, or a daemon/OS service/actual server. This mechanism also permits different drivers (and hardware) to coexist within the same system, as well as allowing several applications to share system resources for audio, including a single hardware output device.

openDevice :: MonadIO m => Maybe String -> m (Maybe Device) 

openDevice allows the application (i.e. the client program) to connect to a device (i.e. the server). If the function returns Nothing, then no sound driver/device has been found. The argument to openDevice specifies a certain device or device configuration.

You can use allDeviceSpecifiers to get a list of the known OpenAL devices.



The syntax of these files is lisp-based and a sequence of expressions, where an expression is one the following:

A boolean value of the form #f or #t, which evaluate to false and true, respectively.
An non-negative integer value, i.e. a sequence of decimal digits, evaluating to itself.
A (signed) floating point value, as recoginzed by C's atof(), evaluating to itself.
A pointer value of the form #pnum, where num can be an octal, decimal or hexadecimal value, as recoginzed by C`s strtol(), evaluating to itself. This kind of expression is currently unused.
A string, i.e. a sequence of printable/whitespace characters between double quotes, evaluating to itself.
A symbol, i.e. a sequence of almost all characters which don't form a simple expression like the ones mentioned below, e.g. foo, bar1, 3baz, ... The symbol evaluates to the value currently bound to it.
A function application of the form (symbol expression...). The function bound to the symbol is applied to the evaluated arguments.
A quotation of the form (quote expression) or 'expression, evaluating to the unevaluated expression itself.
A definition of the form (define symbol expression), binding symbol to the value of expression. The whole expression evaluates to the value of expression, too.
A conjunction of boolean expressions of the form (and expression...). Each expression is evaluated in turn, and if one of them evaluates to false, the value of the whole expression is false. Otherwise the value is true.
An extension loading mechanism of the form (load-extension libraryName), where libraryName has to evaluate to a string. This tries to load the dynamic library with up to 3 special entry points: alExtension_03282000 (pointing to a mandatory NULL-terminated sequence of pairs of pointers to names and extension functions), alExtInit_03282000 (an optional initialization function), and alExtFini_03282000 (an optional cleanup function). If the extension could be loaded successfully, the whole expression evaluates to true, otherwise to false.
Some symbols have a special meaning for OpenAL:

devices
Has the form (devspec...), where devspec is either a symbol/string specifying a device or (device device-param...), specifying a device with additional parameters. These optional device parameters are stored in a variable device-params, but are currently unused. The first device which can successfully opened is used.
direction
Type string or symbol: "read" specifies that the device should be an input device, everything else means output device (default).
sampling-rate
Type integer or float: Specifies the internal mixing frequency, default is 44.1kHz.
speaker-num
Type integer or float: Specifies the number of speakers, which can be 1, 2 (default), or 4.
alsa-device
Type string, alsa backend only: Specifies both alsa-out-device and alsa-in-device, default "plughw:0,0".
alsa-out-device
Type string, alsa backend only: Specifies the ALSA output device, defaults to the value of alsa-device.
alsa-in-device
Type string, alsa backend only: Specifies the ALSA input device, defaults to the value of alsa-device.
native-in-device
Type string, native backend on IRIX only.
native-out-device
Type string, native backend on IRIX only.
native-rear-out-device
Type string, native backend on IRIX only.
native-use-select
Type boolean, native backend on Linux only: If #t, wait up to 0.8sec for the device to become ready for writing. If #f, just try to write and hope it won't hang forever. The latter might be necessary for some drivers which don't implement select() , like some Aureal drivers.
lin-dsp-path
Type string, native backend on Linux only: Path to DSP device for writing, tried before /dev/sound/dsp and /dev/sound if set.
lin-dsp-read-path
Type string, native backend on Linux only: Path to DSP device for reading, tried before /dev/sound/dsp and /dev/sound if set. Defaults to the value of lin-dsp-path.
native-backend-debug
Type boolean, native backend on Darwin only: If set to #f, be a bit verbose on stderr about what's going on in the backend.
source-rolloff-factor
Type integer or float: Value of the initial rolloff factor for sources, default is 1.0.
listener-position
List of 3 integers or floats: Value of the initial listener position, default is (0 0 0).
listener-velocity
List of 3 integers or floats: Value of the initial listener velocity, default is (0 0 0).
listener-orientation
List of 6 integers or floats: Value of the initial listener orientation (at/up), default is (0 0 -1 0 1 0).
The string given to openDevice has to be of the form '((symbol expression) ...), which means basically a sequence of define expressions. Example:

"'((sampling-rate 8000) (native-backend-debug #f))"


-}

