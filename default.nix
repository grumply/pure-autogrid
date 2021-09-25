{ mkDerivation, stdenv, ghc, base, bytestring, pure-elm, 
  pure-random-pcg, pure-maybe, pure-intersection, pure-stream, 
  vector
}:
mkDerivation {
  pname = "pure-autogrid";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base
    pure-elm 
    pure-random-pcg
    pure-maybe
    pure-intersection
    pure-stream
    vector
  ];
  homepage = "github.com/grumply/pure-autogrid";
  license = stdenv.lib.licenses.bsd3;
}
