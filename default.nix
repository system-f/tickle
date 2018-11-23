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
      rev = "5e9ebc1ffa2e40894ed884b637285022278f98e9";
      sha256 = "085bkmbqa34aks2hgfhxkl2vq8x1qrk5n4nzmvp35nqgcc53cksg";
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
