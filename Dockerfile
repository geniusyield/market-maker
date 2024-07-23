FROM ghcr.io/geniusyield/haskell-base-image:9.2.8

# ==================================[ BUILD ]========================================
WORKDIR /MM

RUN cabal update
COPY cabal.project ./
COPY geniusyield-orderbot.cabal ./
COPY geniusyield-annset/geniusyield-annset.cabal geniusyield-annset/
COPY geniusyield-orderbot-framework/geniusyield-orderbot-framework.cabal geniusyield-orderbot-framework/
COPY geniusyield-market-maker/geniusyield-market-maker.cabal geniusyield-market-maker/

RUN cabal update
RUN cabal build geniusyield-dex-api --only-dependencies

COPY . .

RUN cabal build all
# TODO: run tests
# RUN cabal test

# =============================[ SMART ORDER ROUTER ]================================
LABEL org.opencontainers.image.source="https://github.com/geniusyield/market-maker"

# Default values:
# TODO: Add

ENTRYPOINT ["/bin/bash", "./start.sh"]
