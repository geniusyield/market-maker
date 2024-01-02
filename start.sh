#!/bin/bash
echo $PROVIDER_CONFIG >> ~/config.json
echo $MARKET_MAKER_CONFIG >> ~/market-maker-config.json
echo $PAYMENT_SIGNING_KEY >> ~/payment-signing-key.skey
cabal run geniusyield-market-maker-exe -- Run ~/config.json ~/market-maker-config.json
