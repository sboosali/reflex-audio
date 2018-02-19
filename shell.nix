
{ nixpkgs ? import <nixpkgs> {}

, compiler ? "default"

, withProfiling ? false
, withHoogle    ? false 

, doTest        ? false
, doBenchmark   ? false

, development   ? true
}:

/* Usage:

  nix-shell
  cabal configure 
  cabal build
  cabal run

*/

########################################
let

### "IMPORTS"

inherit (nixpkgs) pkgs;
inherit (pkgs)    fetchFromGitHub;

lib = import "${nixpkgs.path}/pkgs/development/haskell-modules/lib.nix" { pkgs = nixpkgs; };
haskell = nixpkgs.haskell.lib;
#haskell = lib;
hs = pkgs.haskell.lib; #TODO rm

in
########################################
let

### UTILITIES

skipTests       = haskell.dontCheck; 
dropUpperBounds = haskell.doJailbreak;

cabal2nixResult = options: src:
  nixpkgs.runCommand "cabal2nixResult" {
    buildCommand = ''
      cabal2nix ${options} file://"${src}" >"$out"
    '';
    buildInputs = with nixpkgs; [
      cabal2nix
    ];
  } "";

in
########################################
let

### SOURCE OVERRIDES

# "megarepos" which have multiple packages as subdirectories.
repositories = {

  reflex-dom = fetchFromGitHub {
    owner           = "reflex-frp";
    repo            = "reflex-dom"; 
    rev             = "212dca4b7ff323dca423f2dd934341bdee7ea2c5";
    sha256          = "0wv8xwr4bv2zb8qz3kf7nq2ixjg2hmyccxppgpwis3wmjai89frk";
  };

};

# 
sources = {

  reflex = fetchFromGitHub {
    owner           = "reflex-frp";
    repo            = "reflex";
    rev             = "8e0177ff28c25436452dba1222cbf8d1a20424fd";
    fetchSubmodules = true;
    sha256          = "1f0xhwq4wvf5c6w8qhvpcn30jaxxq29s2x3iy8bml3a65fpvj0sh";
  };

# PROBLEM
# Warning:
#     This package indirectly depends on multiple versions of the same package. This is very likely to cause a compile failure.
#       package haskell-src-meta (haskell-src-meta-0.8.0.1-1HfhwjlpuugEHqUXKk8ROg) requires haskell-src-exts-1.19.1-GkJUFo8Rp3b1KlAdoTXU6c
#       package reflex (reflex-0.5) requires haskell-src-exts-1.20.1-835K5nW7Qg0K3DUFrUYhiW
# 
# SOLUTION
# -f-use-template-haskell 
#   if flag(use-template-haskell)
#     cpp-options: -DUSE_TEMPLATE_HASKELL
#     build-depends:
#       dependent-sum >= 0.3 && < 0.5,
#       haskell-src-exts >= 1.16 && < 1.21,
#       haskell-src-meta >= 0.6 && < 0.9,
#       template-haskell >= 2.9 && < 2.13
#     exposed-modules:
#       Reflex.Dynamic.TH
#     other-extensions: TemplateHaskell

  # TODO `subdir ? null`
  reflex-dom = fetchFromGitHub {
    owner           = "reflex-frp";
    repo            = "reflex-dom"; 
    rev             = "212dca4b7ff323dca423f2dd934341bdee7ea2c5";
    #fetchSubmodules = true;
    sha256          = "0wv8xwr4bv2zb8qz3kf7nq2ixjg2hmyccxppgpwis3wmjai89frk";
  };

#   # TODO `subdir ? null`
#   reflex-dom = fetchFromGitHub {
#     owner           = "reflex-frp";
#     repo            = "reflex-dom/reflex-dom"; # lol
#     rev             = "212dca4b7ff323dca423f2dd934341bdee7ea2c5";
# #    fetchSubmodules = true;
#     sha256          = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx";
#   };

    # This is where to put the output from nix-prefetch-git
    #
    # This is based on the results o
    #   nix-prefetch-git http://github.com/ekmett/mtl
    #
    # For general git fetching:
    #
    # mtl = fetchgit {
    #   url = "http://github.com/ekmett/mtl";
    #   rev = "f75228f7a750a74f2ffd75bfbf7239d1525a87fe";
    #   sha256= "032s8g8j4djx7y3f8ryfmg6rwsmxhzxha2qh1fj15hr8wksvz42a";
    # };
    #
    # Or, more efficient for github repos:
    #
    # mtl = fetchFromGitHub {
    #   owner = "ekmett";
    #   repo = "mtl";
    #   rev = "f75228f7a750a74f2ffd75bfbf7239d1525a87fe";
    #   sha256= "032s8g8j4djx7y3f8ryfmg6rwsmxhzxha2qh1fj15hr8wksvz42a";
    # };
};

in
########################################
let

### COMPILERS

haskellPackagesWithCompiler = 
  if compiler == "default"
  then pkgs.haskellPackages
  else pkgs.haskell.packages.${compiler};

haskellPackagesWithProfiling = 
  if withProfiling
  then haskellPackagesWithCompiler.override {
         overrides = self: super: {
           mkDerivation = args: super.mkDerivation (args // { enableLibraryProfiling = true; });
         };
       }
  else haskellPackagesWithCompiler;
                 
haskellPackagesWithHoogle =
  if withHoogle
  then haskellPackagesWithProfiling.override {
         overrides = self: super: {
           ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
           ghcWithPackages = self.ghc.withPackages;
         };
       }
  else haskellPackagesWithProfiling;

in

########################################
### Haskell Dependencies...
let

/*

NOTES

* `local` / `github`: 
   They call `import` directly, thus those directories require a `default.nix`
* `cabal2nix` / `hackage` / `github2nix`: 
   They call `cabal2nix`, which generates the `default.nix`, so they don't require the given directory to be a valid `nix` package. 

*/
myHaskellOverlaysWith = self: super: let
#myHaskellOverlaysWith = pkgs: self: super: let

 local      = path:
              self.callPackage path; 

 github     = o:
              self.callPackage (pkgs.fetchFromGitHub o); 

             # o ::
             #      { owner           :: String
             #        repo            :: String
             #        rev             :: String
             #        fetchSubmodules :: Bool
             #        sha256          :: String
             #      } 

 cabal2nix  = name: source: 
              self.callCabal2nix name source;

 hackage    = name: version:
              self.callHackage name version;

 github2nix = o:
              cabal2nix o.repo (pkgs.fetchFromGitHub o); 

 # override the package without overriding any dependencies
 local_      = path:           local      path         {};
 github_     = o:              github     o            {};
 cabal2nix_  = name: source:   cabal2nix  name source  {};
 hackage_    = name: version:  hackage    name version {};
 github2nix_ = o:              github2nix o            {};

 in

 {
   ########################################
   # Add Haskell Packages Below           #
   ######################################## 

  spiros = local_ ../spiros;

    # spiros = github2nix_ {
    #   owner  = "sboosali";
    #   repo   = "spiros";
    #   rev    = "f6c86509cfa1b198c5deb4b89b3dadf6184ea1d0"; 
    #   sha256 = "0bvxgp1mvlpwzr9qdpjs20qs4i813wnhrsl3nq25r8v68x6fblhk";
    # };
      # NOTE
      # latest needs ghc-8.2.2
      # rev "2b7517f27242863ba153bc045dd269b348df05aa" 

 /* 

  # You can use `callHackage` and `callCabal2nix` 
  # to bump package versions or build them from GitHub. 
  # e.g.

    spiros = self.spiros_loose;

    spiros_loose   = skipTests (dropUpperBounds self.spiros_github);
    spiros_local   = local ../spiros {
    };
    spiros_hackage = hackage "spiros" "0.0.0" {
    };
    spiros_github  = github2nix {
      owner  = "sboosali";
      repo   = "spiros";
      rev    = "f6c86509cfa1b198c5deb4b89b3dadf6184ea1d0"; 
      # "2b7517f27242863ba153bc045dd269b348df05aa" 
      # latest needs ghc-8.2.2
      sha256 = 
         "0bvxgp1mvlpwzr9qdpjs20qs4i813wnhrsl3nq25r8v68x6fblhk";
    } {
    };
 */

};

in
########################################
let

### OTHER OVERRIDES
 
modifiedHaskellPackages = haskellPackagesWithHoogle.override {
  overrides = self: super: {


     # reflex     =
     #   self.callPackage (cabal2nixResult "-f-use-template-haskell" sources.reflex) {
     #  };

     # #NOTE `reflex-dom` and `reflex-dom-core` are in the same github repo, called `reflex-dom`
     # reflex-dom-core =
     #   self.callPackage (cabal2nixResult "--subpath reflex-dom-core" sources.reflex-dom) {
     #     inherit (self) reflex;
     #   };

     # reflex-dom =
     #   self.callPackage (cabal2nixResult "--subpath reflex-dom -f-use-warp -fbuild-examples" sources.reflex-dom) {
     #     inherit (self) reflex reflex-dom-core;
     #   };

     # jsaddle-warp =
     #   hs.dontCheck super.jsaddle-warp;
     #   #
     #   # Setup: Encountered missing dependencies:
     #   # doctest >=0.10.1 && <0.12, websockets >=0.9.5.0 && <0.11

     # websockets = 
     #  self.callHackage "websockets" "0.10.0.0" {};

     # exception-transformers =
     #   hs.dontCheck super.exception-transformers;
     #  #       # Setup: Encountered missing dependencies:
     #  #       # HUnit >=1.2 && <1.6
     #  #       # builder for ‘/nix/store/365zv27f15qplgd6gd58fa8v26x2gg5z-exception-transformers-0.4.0.5.drv’ failed with exit code 1

     #  # Add various dependencies here.
     #  #
     #  # Local dependencies:
     #  # my-dependency = self.callPackage ./deps/my-dependency {};
     #  #
     #  # Local dependencies with tests disabled:
     #  # my-dependency = lib.dontCheck (self.callPackage ./deps/my-dependency {});
     #  #
     #  # Git dependencies:
     #  # mtl = self.callPackage (cabal2nixResult sources.mtl) {};
  };
};

in
########################################
let

### DERIVATION / ENVIRONMENT
  
installationDerivation = modifiedHaskellPackages.callPackage ./. {};

# development environment
# for `nix-shell --pure`
developmentDerivation = hs.linkWithGold 
    (hs.addBuildDepends installationDerivation developmentPackages);

developmentPackages = developmentHaskellPackages
                   # ++ developmentEmacsPackages 
                   ++ developmentSystemPackages;

developmentSystemPackages = with pkgs; [
  
 cabal-install

 coreutils
 inotify-tools
  
 emacs
 git

];

developmentHaskellPackages = with modifiedHaskellPackages; [
  
 # ghcid
 # ghc-mod

 stylish-haskell
 hasktags
 present
 hlint
 hoogle
 hindent
  
];

 # developmentHaskellPackages = with Packages; [
 #    dante
 #  ];

environment = hs.shellAware developmentDerivation;
   # if pkgs.lib.inNixShell then drv.env else drv;

in
########################################

environment

########################################
/*

*/
