{ghc}:
with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "myEnv";
  buildInputs = [
      darwin.apple_sdk.frameworks.CoreServices
      libffi
      zlib
      ncurses
      gmp
      pkgconfig
      ] ++ lib.optionals stdenv.isDarwin (with darwin.apple_sdk.frameworks; [
                                                Cocoa
                                                CoreServices
                                                ]) ;
}
