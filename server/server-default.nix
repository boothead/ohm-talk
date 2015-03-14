{ cabal, aeson, engineIo, engineIoSnap, snapServer, snapCors
, socketIo, stm, text, lens
}:

cabal.mkDerivation (self: {
  pname = "revealjs-server";
  version = "1.0.0";
  src = ./.;
  buildDepends = [
    aeson engineIo engineIoSnap snapCors snapServer socketIo text lens
  ];
  meta = {
    homepage = "http://github.com/ocharles/engine.io";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
