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

RUN curl -sSL https://get.haskellstack.org/ | sh

WORKDIR /workspace
