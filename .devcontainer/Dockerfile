FROM haskell:9.2.4

RUN apt-get update \
    && apt-get install -y \
        build-essential \
        curl \
        git \
        libffi-dev \
        libgmp-dev \
        libncurses-dev \
        libtinfo5 \
        m4 \
        pkg-config \
        zlib1g-dev

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

RUN ghcup install hls

RUN curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.3/install.sh | bash

RUN nvm install 18.4.0

WORKDIR /workspace
