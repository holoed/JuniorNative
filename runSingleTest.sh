rm cabal.project.local
cabal install --only-dependencies --overwrite-policy=always
cabal configure --enable-tests --ghc-options="-j"
cabal new-build -j8 
cabal new-test --test-show-details=direct --test-option=--match --test-option=$1
