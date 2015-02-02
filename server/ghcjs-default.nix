{ cabal, aeson }:

cabal.mkDerivation (self: {
  pname = "revealjs-server";
  version = "1.0.0";
  src = ./.;
  buildDepends = [
    aeson
  ];
  meta = {
    homepage = "http://github.com/boothead/ohm-talk";
    license = self.stdenv.lib.licenses.bsd2;
    platforms = self.ghc.meta.platforms;
  };
})
