# Market maker bot

> **⚠️* Warning**
> Market making is a risky activity and running this bot can lead to lose of funds.

Market maker bot for [GeniusYield](https://www.geniusyield.co/) DEX which implements _fixed spread versus market price strategy_.

## Fixed spread vs market price strategy

> **ⓘ Order classification and price**
> We call non-ada token as _commodity_ and ada as _currency_. Order offering currency in exchange of commodity is called as _buy order_ whereas order offering commodity in exchange of currency is called as _sell order_.
> 
> _Price_ is described in display unit[^1] of currency per display unit of commodity token.

Given a market price `M` and a variable `δ` defined as _spread_, bot would place following orders where exact number and volume is determined by configuration:

* Buy orders at price:
  * `M * (1 - δ)`
  * `M * (1 - δ - δ / 2)`
  * `M * (1 - δ - δ / 2 - δ / 2)`
  * And so on, where `n`th buy order's price is given by `M * (1 - δ - (n - 1) * δ / 2)`.
* Sell orders at price:
  * `M * (1 + δ)`
  * `M * (1 + δ + δ / 2)`
  * `M * (1 + δ + δ / 2 + δ / 2)`
  * And so on, where `n`th sell order's price is given by `M * (1 + δ + (n - 1) * δ / 2)`.

If market price has drifted way higher (_"way higher"_ as directed by configuration) than the price at which buy orders were placed, buy orders would be canceled. Likewise, if price has drifted way lower than the price at which sell orders were placed, they would be canceled.

## Running the market maker bot

First, you need to setup the necessary tooling to work with [haskell.nix](https://github.com/input-output-hk/haskell.nix), then simply run `nix develop`, and it will drop you into a shell with all the necessary tools. Once inside the environment, you can build the order bot with `cabal build all`.

> **ⓘ **
> Nix is not necessary if your environment already has the right set of dependencies. One may look at the [CI file](https://github.com/geniusyield/atlas/blob/main/.github/workflows/haskell.yml) for our transaction building tool, which current project also relies on, to see dependencies used.

Then the bot can be ran with following command: `cabal run geniusyield-market-maker-exe -- Run my-atlas-config.json my-maker-bot-config.json` where `my-atlas-config.json` is the configuration for [Atlas](https://github.com/geniusyield/atlas) and `my-maker-bot-config.json` is the configuration of our market maker bot.

See [`atlas-config-maestro.json`](./atlas-config-maestro.json) & [`atlas-config-kupo.json`](./atlas-config-kupo.json) as an example of Atlas configuration using [Maestro](https://www.gomaestro.org/) provider & local node with [Kupo](https://github.com/CardanoSolutions/kupo) respectively.

### Bot configuration

> **ⓘ**
> See [`sample-preprod-maker-bot-config-gens.json`](./sample-preprod-maker-bot-config-gens.json) and [`sample-mainnet-maker-bot-config-gens.json`](./sample-mainnet-maker-bot-config-gens.json) for sample Preprod and Mainnet market maker bot configuration respectively.

```json
{
  "mbc_user": {
    "ur_s_key_path": "path-to-skey",
    "ur_coll": "tx-id#tx-ix"
  },
  "mbc_fp_nft_policy": "compiled-scripts/minting-policy",
  "mbc_fp_order_validator": "compiled-scripts/partial-order",
  "mbc_po_config_addr": "addr_test1wrgvy8fermjrruaf7fnndtmpuw4xx4cnvfqjp5zqu8kscfcvh32qk",
  "mbc_po_refs": {
    "por_ref_nft": "fae686ea8f21d567841d703dea4d4221c2af071a6f2b433ff07c0af2.8309f9861928a55d37e84f6594b878941edce5e351f7904c2c63b559bde45c5c",
    "por_val_ref": "be6f8dc16d4e8d5aad566ff6b5ffefdda574817a60d503e2a0ea95f773175050#2",
    "por_mint_ref": "be6f8dc16d4e8d5aad566ff6b5ffefdda574817a60d503e2a0ea95f773175050#1"
  },
  "mbc_delay": 120000000,
  "mbc_price_config": {
    "pc_api_key": "<<MAESTRO_TOKEN>>",
    "pc_network_id": "mainnet",
    "pc_dex": "genius-yield",
    "pc_override": {
      "mpo_commodity_token": "c6e65ba7878b2f8ea0ad39287d3e2fd256dc5c4160fc19bdf4c4d87e.7447454e53",
      "mpo_pair": "ADA-GENS",
      "mpo_commodity_is_first": false
    }
  },
  "mbc_strategy_config": {
    "sc_spread": {
      "numerator": 1,
      "denominator": 100
    },
    "sc_token_volume": {
      "tv_sell_min_vol": 1000000000,
      "tv_buy_min_vol": 2000000000,
      "tv_sell_budget": 3500000000,
      "tv_buy_budget": 6600000000,
      "tv_sell_vol_threshold": 10000000000,
      "tv_buy_vol_threshold": 20000000000
    },
    "sc_price_check_product": 9,
    "sc_cancel_threshold_product": 4
  },
  "mbc_token": {
    "stAc": "c6e65ba7878b2f8ea0ad39287d3e2fd256dc5c4160fc19bdf4c4d87e.7447454e53",
    "stPrecision": 6
  }
}
```
* `mbc_user` describes individual bot, it specifies `ur_s_key_path` which is the path to signing key file and `ur_coll` which is the UTxO reserved as collateral.
* Fields `mbc_fp_nft_policy`, `mbc_fp_order_validator`, `mbc_po_config_addr` and `mbc_po_refs` relate to DEX smart contracts and can be left as it is.
* `mbc_delay`: Bot in single iteration tries to determine which orders need to be placed and which are needed to be cancelled. Once determined, it tries building the transactions and proceeds with submitting them, completing this single iteration. `mbc_delay` determines time in microseconds that bot must wait before proceeding with next iteration.
* `mbc_price_config` gives the configuration on how to get market price using https://docs.gomaestro.org/DefiMarketAPI/mkt-dex-ohlc Maestro endpoint, for a token.
  * `pc_api_key` is the Maestro API key.
  * `pc_network_id` determines Cardano network which is mentioned for in API calls. It should always be kept `mainnet` as of now.
  * `pc_dex` determines DEX from which market price is queried for. Currently `minswap` & `genius-yield` are supported.
  * `pc_override` is optional and is needed in case one is not running bot on Mainnet. Since tokens on test network aren't actively traded, their price is not returned for by Maestro endpoint. To still get mainnet price for them, one can override the token given by `mpo_commodity_token` to pair with commodity token as described by `mpo_pair` & `mpo_commodity_is_first` respectively. In the above configuration, we are overriding the testnet GENS asset class `c6e65ba7878b2f8ea0ad39287d3e2fd256dc5c4160fc19bdf4c4d87e.7447454e53`, for the mainnet token pair `ADA-GENS`, and GENS is the second token in the pair so `mpo_commodity_is_first` is set to **false**. If the pair instead was `GENS-ADA` then `mpo_commodity_is_first` should be set to **true**.
* `mbc_strategy_config` determines parameters for strategy:

  * `sc_spread`: Ratio representing `δ` as described before.

## Canceling all the orders

If you want to cancel orders placed by the simulator you can run `cabal run geniusyield-market-maker-exe -- Cancel my-atlas-config.json my-maker-bot-config.json`.

[^1]: _Display unit_ is one to which decimals are added as directed under [`cardano-token-registry`](https://github.com/cardano-foundation/cardano-token-registry).
