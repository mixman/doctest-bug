
DOCTEST_OPTIONS:= --fast \
-XDeriveAnyClass \
-XDeriveFoldable \
-XDeriveFunctor \
-XDeriveGeneric \
-XDeriveTraversable \
-XDerivingStrategies \
-XGeneralizedNewtypeDeriving \
-XScopedTypeVariables \
-XStandaloneDeriving

doctest: doctest-bug

doctest-bug:
	cabal v2-build --write-ghc-environment-files=always --enable-tests -j2 all
	cabal v2-test --enable-tests all
	doctest $(DOCTEST_OPTIONS) src
