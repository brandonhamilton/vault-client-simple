let
  sources = import ./nix/sources.nix;
  compilerVersion = "ghc8101";
  hnix = import sources.iohk-hnix { };
  pkgs = (import hnix.sources.nixpkgs-2003) hnix.nixpkgsArgs;
in pkgs.haskell-nix.cabalProject {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "vault-client-simple";
    src = ./.;
  };
  compiler-nix-name = compilerVersion;
}
