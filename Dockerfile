FROM haskell:9.2.8-slim as builder

ENV LANG C.UTF-8

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        autoconf \
        automake \
        build-essential \
        chrony \
        libncursesw5 \
        liblzma-dev \
        libpq-dev \
        libssl-dev \
        libsystemd-dev \
        libtool \
        pkg-config \
        procps \
        tmux && \
    rm -rf /var/lib/apt/lists/*

# Libsodium:
RUN git clone https://github.com/input-output-hk/libsodium && \
    cd libsodium && \
    git checkout dbb48cc && \
    ./autogen.sh && \
    ./configure && \
    make && \
    make install

# Libsecp256k1:
RUN git clone https://github.com/bitcoin-core/secp256k1 && \
    cd secp256k1 && \
    git checkout ac83be33d0956faf6b7f61a60ab524ef7d6a473a && \
    ./autogen.sh && \
    ./configure --prefix=/usr --enable-module-schnorrsig --enable-experimental && \
    make && \
    make install

ENV LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
ENV PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"

# ==================================[ BUILD ]========================================
WORKDIR /MM

RUN cabal update
COPY cabal.project ./
COPY geniusyield-orderbot.cabal ./
COPY geniusyield-annset/geniusyield-annset.cabal geniusyield-annset/
COPY geniusyield-orderbot-framework/geniusyield-orderbot-framework.cabal geniusyield-orderbot-framework/
COPY geniusyield-market-maker/geniusyield-market-maker.cabal geniusyield-market-maker/

RUN cabal update
RUN cabal install geniusyield-dex-api --only-dependencies

COPY . .

RUN cabal build all
# TODO: run tests
# RUN cabal test

# =============================[ SMART ORDER ROUTER ]================================
LABEL org.opencontainers.image.source="https://github.com/geniusyield/market-maker"

# Default values:
# TODO: Add

ENTRYPOINT ["/bin/bash", "./start.sh"]
