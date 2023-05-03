FROM haskell:8.10

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
RUN curl -L -o haskell-language-server.tar.gz https://github.com/haskell/haskell-language-server/releases/download/1.4.0/haskell-language-server-1.4.0-linux.tar.gz \
 && tar xf haskell-language-server.tar.gz \
 && rm haskell-language-server.tar.gz \
 && mv haskell-language-server-*-linux /usr/local/bin/haskell-language-server
