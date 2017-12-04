{ pkgs ? import <nixpkgs> {}, ghc ? pkgs.ghc }:

pkgs.haskell.lib.buildStackProject {
  name = "yannesposito.com";
  inherit ghc;
  buildInputs = with pkgs; [ darwin.apple_sdk.frameworks.CoreServices
                             gcc
                             libffi
                             zlib
                             ncurses
                             gmp
                             pkgconfig
                             git
                             gnupg
                             sassc] ++
                             lib.optionals stdenv.isDarwin
                                           (with darwin.apple_sdk.frameworks; [ Cocoa CoreServices ]) ;
  LC_ALL = "en_US.UTF-8";
  LANG = "en_US.UTF-8";
  TMPDIR = "/tmp";
}
