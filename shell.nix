let
  sources = import ./nix/sources.nix;
  compilerVersion = "ghc8101";
  hnix = import sources.iohk-hnix { };
  pkgs = (import hnix.sources.nixpkgs-2003) hnix.nixpkgsArgs;
in (import ./.).shellFor {
  withHoogle = false;
  tools = {
    cabal = "3.2.0.0";
    hlint = "2.2.11";
    ghcid = "0.8.7";
  };
  buildInputs = [ pkgs.zlib ];
  exactDeps = true;
}
