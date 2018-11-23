{ supportedSystems ? ["x86_64-linux"]
, supportedCompilers ? [ "ghc802" "ghc822" "ghc843" ] 
}:

with (import <nixpkgs/pkgs/top-level/release-lib.nix> { inherit supportedSystems; });

let
  pkgs = import <nixpkgs> {};

  configurations = 
    pkgs.lib.listToAttrs (
      pkgs.lib.concatMap (compiler: 
        pkgs.lib.concatMap (system: 
          [{name = "haskell-packages-" + compiler + "-tickle-" + system ; value = {inherit compiler system;};}]
        ) supportedSystems
      ) supportedCompilers
    );

  jobs =
      pkgs.lib.mapAttrs (name: configuration: 
          let
            compiler = configuration.compiler; 
            system = configuration.system; 
            nixpkgs = { pkgs = pkgsFor system; };
            tickle = import ../default.nix { inherit nixpkgs compiler; };
          in
            tickle
      ) configurations;
in
  jobs
