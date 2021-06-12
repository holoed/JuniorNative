FROM haskell:8.10.4

RUN cabal update

# Add .cabal file
ADD ./JuniorNative.cabal /opt/JuniorNative.cabal

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
RUN cd /opt && cabal install --only-dependencies -j4

# Add and Install Application Code
ADD . /opt
RUN cd /opt && cabal configure && cabal build JuniorService && cabal install

# Add installed cabal executable to PATH
ENV PATH /root/.cabal/bin:$PATH

# Default Command for Container
WORKDIR /opt

ENTRYPOINT ["JuniorService"]
