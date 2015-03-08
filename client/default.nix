{ cabal, aeson, ghcjsBase, ghcjsDom, ghcjsPrim, lens, mvc, oHm
, revealJSServer, virtualDom, pipes, pipesConcurrency, profunctors, stm, time
}:

cabal.mkDerivation (self: {
  pname = "revealjs-client";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    aeson ghcjsBase ghcjsDom ghcjsPrim lens mvc virtualDom oHm revealJSServer pipes
    pipesConcurrency profunctors stm time
  ];
  doCheck = false;
  meta = {
    license = self.stdenv.lib.licenses.unfree;
    platforms = self.ghc.meta.platforms;
  };
})
