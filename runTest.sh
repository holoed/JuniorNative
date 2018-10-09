cabal install --only-dependencies
cabal configure --enable-tests --ghc-options="-j"
cabal build -j8
cabal test --show-details=always --test-option=--color
