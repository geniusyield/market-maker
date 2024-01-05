#!/bin/bash
echo $PROVIDER_CONFIG >> ~/config.json
echo $MARKET_MAKER_CONFIG >> ~/market-maker-config.json
echo $PAYMENT_SIGNING_KEY >> ~/payment-signing-key.skey
if [[ "${MODE:=MM}" == "CANCEL" ]]; then
  echo "Canceling all the orders..."
  cabal run geniusyield-market-maker-exe -- Cancel ~/config.json ~/market-maker-config.json
else
  echo "Starting market maker..."
  cabal run geniusyield-market-maker-exe -- Run ~/config.json ~/market-maker-config.json
fi
