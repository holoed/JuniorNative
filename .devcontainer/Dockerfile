# Use the official Ubuntu 20.04 image as the base
FROM ubuntu:20.04

# Install necessary dependencies
RUN apt-get update && \
    apt-get install -y \
        build-essential \
        curl \
        git \
        libffi-dev \
        libffi7 \
        libgmp-dev \
        libgmp10 \
        libncurses-dev \
        libncurses5 \
        libtinfo5 \
        zlib1g-dev \
        zlib1g

# Install GHCUP
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Set environment variables for GHCUP
ENV PATH="/root/.ghcup/bin:$PATH"

# Install GHC 9.2.4 and Haskell Language Server
RUN ghcup install ghc 9.2.4 && \
    ghcup set ghc 9.2.4 && \
    ghcup install hls && \
    ghcup set hls

# Install Node.js 18.4.0 using Node Version Manager (NVM)
RUN curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.1/install.sh | bash
ENV NVM_DIR="/root/.nvm"
ENV PATH="/root/.nvm/versions/node/v18.4.0/bin:$PATH"
RUN . $NVM_DIR/nvm.sh && \
    nvm install 18.4.0 && \
    nvm use 18.4.0 && \
    nvm alias default 18.4.0

# Expose the default port for Haskell applications
EXPOSE 8080

WORKDIR /workspace
