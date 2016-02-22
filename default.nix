{ mkDerivation, base, stdenv, vulkan}:

mkDerivation {
  pname = "vulkan-examples";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [ base vulkan ];
  testHaskellDepends = [ base ];
  homepage = "http://github.com/expipiplus1/vulkan-examples#readme";
  description = "Initial project template from stack";
  license = stdenv.lib.licenses.bsd3;
}
