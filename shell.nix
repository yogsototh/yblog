with import <nixpkgs> {};

mkShell {
  name = "yannesposito.com";
  inherit ghc;
  buildInputs = [ gnupg sassc];
  LC_ALL = "en_US.UTF-8";
  LANG = "en_US.UTF-8";
  TMPDIR = "/tmp";
}
