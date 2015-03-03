with (import <nixpkgs> {}).pkgs;
let
    hsPackages = haskell-ng.packages.ghcjs.override {
      overrides = self: super: {
        virtual-dom = self.callPackage ./virtual-dom {};
        oHm = self.callPackage ./oHm {};
        revealjs-server = self.callPackage
          ({ mkDerivation, aeson, base, containers, ghcjs-base }:
           mkDerivation {
             pname = "revealjs-server";
             version = "1.0.0.3";
             src = ../server;
             isLibrary = true;
             isExecutable = true;
             buildDepends = [ aeson base containers ghcjs-base ];
             homepage = "http://github.com/boothead/ohm-talk";
             license = stdenv.lib.licenses.bsd2;
           }) {};
        revealjs-client = self.callPackage ./. {};
      };
    };
        
in
  hsPackages.revealjs-client.env
