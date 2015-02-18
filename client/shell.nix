with import <nixpkgs> {};
let haskellPackages = pkgs.haskellPackages_ghcjs.override {
      extension = self: super: {
        oHm = self.callPackage ./oHm {};
        revealJSServer = self.callPackage ../server {};
        client = self.callPackage ./. {};
      };
    };
 
in pkgs.callPackage ./. {
     cabal = haskellPackages.cabal.override {
       extension = self: super: { 
         buildTools = super.buildTools ++ [ haskellPackages.ghc.ghc.parent.cabalInstall ]; 
       };
     }; 
     inherit (haskellPackages) aeson ghcjsBase ghcjsDom ghcjsPrim oHm revealJSServer lens mvc pipes
                               pipesConcurrency profunctors stm;
     
   }
