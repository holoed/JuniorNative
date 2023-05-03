FROM haskell:9.0.2

# Install additional tools and dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    curl \
    git \
    libffi-dev \
    libgmp-dev \
    libncurses-dev \
    libtinfo5 \
    zlib1g-dev \
 && rm -rf /var/lib/apt/lists/*

# Install the Haskell Language Server
RUN curl -L -o haskell-language-server.tar.gz https://downloads.haskell.org/~hls/haskell-language-server-1.10.0.0/haskell-language-server-1.10.0.0-x86_64-linux-ubuntu18.04.tar.xz \
 && tar xf haskell-language-server.tar.gz \
 && rm haskell-language-server.tar.gz \
 && mv haskell-language-server-*-linux /usr/local/bin/haskell-language-server
