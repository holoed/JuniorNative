cabal install --only-dependencies
cabal configure --enable-tests
cabal build
cabal test --show-details=always --test-option=--color
