{ cabal, binary, fastLogger, hspec, liftedBase, monadControl
, monadLogger, pcreLight, text, time, transformers, vectorSpace
}:

cabal.mkDerivation (self: {
  pname = "logging";
  version = "1.3.0";
  src = ./.;
  buildDepends = [
    binary fastLogger liftedBase monadControl monadLogger pcreLight
    text time transformers vectorSpace
  ];
  testDepends = [ hspec monadLogger ];
  meta = {
    description = "Simplified logging in IO for application writers";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
