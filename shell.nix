with import nix/nixpkgs.nix {};

mkShell {
  name = "yannesposito.com";
  buildInputs = [ gnupg
                  sassc
                  hello
                ];
  LC_ALL = "en_US.UTF-8";
  LANG = "en_US.UTF-8";
  TMPDIR = "/tmp";
}
