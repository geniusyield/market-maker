#!/bin/bash
echo "Initializing market maker..."
echo $PROVIDER_CONFIG >> ~/config.json
echo $MARKET_MAKER_CONFIG >> ~/market-maker-config.json

if [[ -n "$PAYMENT_SIGNING_KEY" ]]; then
  echo " -> Storing payment signing key..."
  echo $PAYMENT_SIGNING_KEY >> ~/payment-signing-key.skey
fi

if [[ "${MODE:=MM}" == "CANCEL" ]]; then
  echo "Canceling all the orders..."
  set -x
  cabal run geniusyield-market-maker-exe -- Cancel ~/config.json ~/market-maker-config.json
else
  echo "Starting market maker..."
  set -x
  cabal run geniusyield-market-maker-exe -- Run ~/config.json ~/market-maker-config.json
fi
