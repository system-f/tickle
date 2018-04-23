{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  sources = {
    papa = pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "papa";
      rev = "9752ec3341df8117121c23e2fa8eadc38af7841b";
      sha256 = "05g00hjd8hi5lvc9k9smqh6s4sjyd07hr94qicjsigl5k241gibh";
    };
  };

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: import sources.papa self // {
      parsers = pkgs.haskell.lib.dontCheck super.parsers;
      tagsoup-selection = pkgs.haskell.lib.doJailbreak super.tagsoup-selection;
      validation = super.validation_1;
    };
  };

  tickle = modifiedHaskellPackages.callPackage ./tickle.nix {};

in
  tickle
