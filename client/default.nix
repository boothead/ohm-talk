{ mkDerivation, aeson, base, containers, ghcjs-base, ghcjs-dom
, ghcjs-prim, lens, mtl, mvc, oHm, pipes, pipes-concurrency
, profunctors, revealjs-server, stdenv, stm, text, time
, transformers
}:
mkDerivation {
  pname = "revealjs-client";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    aeson base containers ghcjs-base ghcjs-dom ghcjs-prim lens mtl mvc
    oHm pipes pipes-concurrency profunctors revealjs-server stm text
    time transformers
  ];
  license = stdenv.lib.licenses.unfree;
}
