{ mkDerivation, base, binary, bytestring, fast-logger, hspec
, lifted-base, monad-control, monad-logger, old-locale, pcre-light
, stdenv, text, time, transformers, unix
}:
mkDerivation {
  pname = "logging";
  version = "2.2.0";
  src = ./.;
  buildDepends = [
    base binary bytestring fast-logger lifted-base monad-control
    monad-logger old-locale pcre-light text time transformers
  ];
  testDepends = [ base hspec monad-logger unix ];
  description = "Simplified logging in IO for application writers";
  license = stdenv.lib.licenses.mit;
}
