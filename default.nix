let
  
  ghcVersion = "ghc865";
  nixpkgsRev = "2d149fcaf3b794947eeb3899859a371d10d38f9f";
  # https://github.com/cachix/ghcide-nix/blob/master/nix/sources.json
  
  githubTarball = owner: repo: rev:
    builtins.fetchTarball { url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz"; };

  pkgs = import (githubTarball "NixOS" "nixpkgs-channels" nixpkgsRev) { inherit config; };
  
  ghcide = import (githubTarball "cachix" "ghcide-nix" "master") {};
  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  extra-deps = with pkgs.haskell.lib; (super: {
    # stylish-haskell
    stylish-haskell = super.callHackageDirect { pkg = "stylish-haskell"; ver = "0.11.0.0"; sha256 = "1a6jijj1lxmi20m9ddiwlnlf3x6qy3dw4si1pvfk9rpjv9azcydk"; } {};
    HsYAML = super.callHackageDirect { pkg = "HsYAML"; ver = "0.2.1.0"; sha256 = "0r2034sw633npz7d2i4brljb5q1aham7kjz6r6vfxx8qqb23dwnc"; } {};
  });
  
  packages = {
    wai-static-th = ./.;
  };

  config = {
    allowBroken = true;
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskell.packages."${ghcVersion}".override {
        overrides = self: super: (extra-deps super) // builtins.mapAttrs (name: path: super.callCabal2nix name (gitignore path) {}) packages;
      };
    };
  };

  shell = with pkgs; haskellPackages.shellFor {
    packages = p: builtins.attrValues (builtins.mapAttrs (name: path: builtins.getAttr name haskellPackages) packages); 
    buildInputs = [
      haskellPackages.cabal-install
      ghcide."ghcide-${ghcVersion}"
      stylish-haskell
      
      (writeShellScriptBin "testLib" '' 
        ${haskellPackages.cabal-install}/bin/cabal test --enable-library-profiling --test-show-details=direct $1 ''${@:2}
      '')

      (writeShellScriptBin "watchLib" ''
        ${ghcid}/bin/ghcid -c "cabal v2-repl $1" -W ''${@:2}
      '')
      
      (writeShellScriptBin "watchExe" '' 
        ${ghcid}/bin/ghcid -c "cabal v2-repl exe:$1" -W -T Main.main ''${@:2}
      '')

    ];
  };
  
in {
  inherit pkgs;
  inherit shell;
}
