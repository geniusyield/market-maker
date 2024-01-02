# Market maker bot

> **⚠️  Warning**
>
> Market making is a risky activity and running this bot can lead to loss of funds.

Market maker bot for [GeniusYield](https://www.geniusyield.co/) DEX which implements _fixed spread versus market price strategy_.

## Fixed spread vs market price strategy

> **ⓘ Order classification and price**
>
> We call non-ada token as _commodity_ and ada as _currency_. Order offering currency in exchange of commodity is called as _buy order_ whereas order offering commodity in exchange of currency is called as _sell order_.
> 
> _Price_ is described in display unit[^1] of currency token per display unit of commodity token.

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

## Running the market maker bot: Using docker compose

The simplest way to start an MM bot can be started using Docker compose.

After cloning the repository a few environment variables must be set. After this has been done; the MM bot container can be started using `docker compose`:

```
git clone git@github.com:geniusyield/market-maker.git
cd market-maker
export MAESTRO_API_KEY=aBcDefghijoXj3v0LB3xvyWoGEfPrP4Vf2
export PAYMENT_SIGNING_KEY='{ "type": "PaymentSigningKeyShelley_ed25519", "description": "Payment Signing Key", "cborHex": "4210268dsb850d08s83a4cf5a4408240248ea571a65bb22bf443586c233ae56bc340" }'
export COLLATERAL_UTXO=d235edd34566a425668a475963dfc2c1c323a11287340b202c35093433491df#0
docker compose up
```

As in the example above; the following environment variables must be specified before calling `docker compose up`:
- `MAESTRO_API_KEY`: The MAINNET API key to be used for accessing the Maestro services.
- `PAYMENT_SIGNING_KEY`: The payment signing key to be used. Please see the [signing key generator](https://github.com/geniusyield/signing-key-generator) for details.
- `COLLATERAL_UTXO`: A suitable UTxO with 5 ADA to be used as colletaral UTxO.

**Please make sure to adapt the `MARKET_MAKER_CONFIG` configuration according to your needs!**

## Running the market maker bot: Building from source

First, you need to setup the necessary tooling to work with [haskell.nix](https://github.com/input-output-hk/haskell.nix), then simply run `nix develop`, and it will drop you into a shell with all the necessary tools. Once inside the environment, you can build the order bot with `cabal build all`.

> **ⓘ**
>
> Nix is not necessary if your environment already has the right set of dependencies. One may look at the [CI file](https://github.com/geniusyield/atlas/blob/main/.github/workflows/haskell.yml) for our transaction building tool, which current project also relies on, to see dependencies used.

Then the bot can be ran with following command: `cabal run geniusyield-market-maker-exe -- Run my-atlas-config.json my-maker-bot-config.json` where `my-atlas-config.json` is the configuration for [Atlas](https://github.com/geniusyield/atlas) and `my-maker-bot-config.json` is the configuration of our market maker bot.

See [`atlas-config-maestro.json`](./atlas-config-maestro.json) & [`atlas-config-kupo.json`](./atlas-config-kupo.json) as an example of Atlas configuration using [Maestro](https://www.gomaestro.org/) provider & local node with [Kupo](https://github.com/CardanoSolutions/kupo) respectively.

### Bot configuration

> **ⓘ**
>
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
* `mbc_delay` - Bot in single iteration tries to determine which orders need to be placed and which are needed to be cancelled. Once determined, it tries building the transactions and proceeds with submitting them, completing this single iteration. `mbc_delay` determines time in microseconds that bot must wait before proceeding with next iteration.
* `mbc_price_config` gives the configuration on how to get market price using https://docs.gomaestro.org/DefiMarketAPI/mkt-dex-ohlc Maestro endpoint, for a token.
  * `pc_api_key` is the Maestro API key.
  * `pc_network_id` determines Cardano network which is mentioned for in API calls. It should always be kept `mainnet` as of now.
  * `pc_dex` determines DEX from which market price is queried for. Currently `minswap` & `genius-yield` are supported.
  * `pc_override` is optional and is needed in case one is not running bot on Mainnet. Since tokens on test network aren't actively traded, their price is not returned for by Maestro endpoint. To still get mainnet price for them, one can override the token given by `mpo_commodity_token` to pair with commodity token as described by `mpo_pair` & `mpo_commodity_is_first` respectively. In the above configuration, we are overriding the testnet GENS asset class `c6e65ba7878b2f8ea0ad39287d3e2fd256dc5c4160fc19bdf4c4d87e.7447454e53`, for the mainnet token pair `ADA-GENS`, and GENS is the second token in the pair so `mpo_commodity_is_first` is set to **false**. If the pair instead was `GENS-ADA` then `mpo_commodity_is_first` should be set to **true**.
* `mbc_strategy_config` determines parameters for strategy:

  * `sc_spread` - Ratio representing `δ` as described before.
  * `sc_cancel_threshold_product` - If the price in buy order is less than `(1 - sc_cancel_threshold_product * δ) * M`, then it is canceled. Likewise if the price in sell order is greater than `(1 + sc_cancel_threshold_product * δ) * M` then it is canceled.
  * `sc_token_volume` specifies following:
    * `tv_sell_min_vol` - Amount of commodity tokens (in lowest possible denomination) that order must at least offer.
    * `tv_buy_min_vol` - Amount of currency tokens (in lovelaces) that order must at least offer.
    * `tv_sell_budget` - Total amount of commodity tokens that bot can cumulatively offer in the orders. In every iteration, bot determines the number of commodity tokens locked in the orders and subtracts it from `tv_sell_budget` field, let's call the obtained number `asb` (short for _available sell budget_) then it determines number of sell orders placed to be `⌊asb / tv_sell_min_vol⌋ = ns` where `ns` is short of number of sell orders. Now bot would place `ns` sell orders, each having offer amount as `⌊asb / ns⌋`.
    * `tv_buy_budget` - Total amount of currency tokens that bot can cumulatively offer in the orders. It governs bot symmetric to `tv_sell_budget`.
    * `tv_sell_vol_threshold` - this is related to `sc_price_check_product`. Bot would build an order book from all the orders for the given pair in GeniusYield DEX. It will sum the offered commodity tokens for sell orders which have price less than `M * (1 + sc_price_check_product * δ)` to get `SV` (short for sell volume) and sum the asked commodity tokens for buy orders which have price greater than `M * (1 + sc_price_check_product * δ)` to get `BV'` (short for buy volume in commodity token). We'll multiply `BV'` with `M` to get `BV` to represent buy volume in currency token. Now, bot would not place a new sell order, if `tv_sell_vol_threshold` is less than or equal to `SV`. Idea is that if there is enough liquidity than bot need not place orders. Symmetrically, bot would not place new buy orders only if `tv_buy_vol_threshold` is less than or equal to `BV`.

## Canceling all the orders

If you want to cancel orders placed by the simulator you can run `cabal run geniusyield-market-maker-exe -- Cancel my-atlas-config.json my-maker-bot-config.json`.

## Known Issues

* When bot tries to place multiple orders in a single iteration, it might happen that we pick same UTxO against different transaction skeletons (due to a [quirk](https://github.com/geniusyield/dex-contracts-api/blob/cf360d6c1db8185b646a34ed8f6bb330c23774bb/src/GeniusYield/Api/Dex/PartialOrder.hs#L489-L498) where place order operation specifies UTxO to be spent in skeleton itself), leading to successful building of only some of the transaction skeletons and thus only few of the orders might be successfully placed even though bot might very well have the required funds to place all. Now bot can place remaining ones in next iteration but as of now, these next orders are placed starting with initial spread difference from market price leading to a situation where bot might have multiple orders at the same price.

[^1]: _Display unit_ is one to which decimals are added as directed under [`cardano-token-registry`](https://github.com/cardano-foundation/cardano-token-registry).
