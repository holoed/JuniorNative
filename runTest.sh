rm cabal.project.local
cabal update
cabal install --only-dependencies --overwrite-policy=always
cabal configure --ghc-options="-j"
cabal new-build -j8 
cabal run TestSuite -- --print
