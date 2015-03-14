{ cabal, aeson, lens }:

cabal.mkDerivation (self: {
  pname = "revealjs-server";
  version = "1.0.0.1";
  src = ./.;
  buildDepends = [
    aeson lens
  ];
  meta = {
    homepage = "http://github.com/boothead/ohm-talk";
    license = self.stdenv.lib.licenses.bsd2;
    platforms = self.ghc.meta.platforms;
  };
})
