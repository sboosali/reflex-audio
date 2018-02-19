[![Build Status](https://secure.travis-ci.org/sboosali/reflex-audio.svg)](http://travis-ci.org/sboosali/reflex-audio)
[![Hackage](https://img.shields.io/hackage/v/reflex-audio.svg)](https://hackage.haskell.org/package/reflex-audio)

# reflex-audio

TODO 

## Example

```
import Reflex.Audio

-- TODO
```

## Links

[Hackage](https://hackage.haskell.org/package/reflex-audio)

[Example module source](https://hackage.haskell.org/package/reflex-audio/docs/src/Reflex-Audio-Example.html). 

## Development

### Nix

These nix files:

* `reflex-audio-default.nix` 
* `reflex-audio-shell.nix` 

were automatically created by the `cabal2nix` command, and can be regenerated with `./reconfigure.sh`. They shouldn't be edited manually. 

While the other nix files:

* `default.nix` 
* `shell.nix` 

import their respecitve autogen'd files, but can (and should) be edited manually when necessary. 

If, you've forked this and have rewritten your own custom nix file for development (e.g. you're on OSX, or you need an older compiler version), please suffix it with that environment (e.g. `osx-shell.nix` or `ghc-7-10-03.nix`), and submit a pull request. Ditto for `stack.yaml` files. 

