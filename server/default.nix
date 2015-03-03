{ mkDerivation, aeson, base, containers, engine-io, engine-io-snap
, mtl, snap-core, snap-cors, snap-server, socket-io, stdenv, stm
, text, transformers
}:
mkDerivation {
  pname = "revealjs-server";
  version = "1.0.0.3";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson base containers engine-io engine-io-snap mtl snap-core
    snap-cors snap-server socket-io stm text transformers
  ];
  homepage = "http://github.com/boothead/ohm-talk";
  license = stdenv.lib.licenses.bsd2;
}
