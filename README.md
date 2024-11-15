
<h1 align="center">Market Maker Bot</h1>
<p align="center">
  <a href="https://github.com/geniusyield/market-maker/actions?query=branch%3Amain"><img src="https://img.shields.io/github/actions/workflow/status/geniusyield/market-maker/build.yml?style=flat-square&branch=main&label=Build" /></a>
  <a href="https://www.haskell.org/"><img alt="GitHub top language" src="https://img.shields.io/github/languages/top/geniusyield/market-maker?style=flat-square"></a>
  <a href="https://github.com/geniusyield/market-maker/commits/main"><img src="https://img.shields.io/github/commit-activity/m/geniusyield/market-maker?style=flat-square&label=Commit%20Activity" /></a>
  <a href="https://github.com/geniusyield/market-maker/blob/main/LICENSE"><img alt="GitHub License" src="https://img.shields.io/github/license/geniusyield/market-maker?label=License&style=flat-square" /></a>
  <a href="./CONTRIBUTING.md"><img src="https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=flat-square" /></a>
  <a href="https://twitter.com/GeniusyieldO"><img src="https://img.shields.io/badge/-%40GeniusYieldO-F3F1EF?style=flat-square&logo=twitter&logoColor=1D9BF0" /></a>
  <a href="https://discord.gg/TNHf4fs626"><img src="https://img.shields.io/badge/-Discord-414EEC?style=flat-square&logo=discord&logoColor=white" /></a>
</p>


> [!WARNING]
> Market making is a risky activity and running this bot can lead to loss of funds.

Market maker bot for [GeniusYield](https://www.geniusyield.co/) DEX which implements _fixed spread versus market price strategy_.

## Fixed spread vs market price strategy

> [!NOTE]
> **Order classification and price**
>
> In the following, we call any non-ADA token _commodity_, and we call ADA _currency_. An order offering currency in exchange for commodity is called a _buy order_, whereas an order offering commodity in exchange for currency is called a _sell order_.
>
> _Price_ is described in display unit[^1] of currency token per display unit of commodity token.

Given a market price `M` and a variable `δ` defined as _spread_, the bot would place the following orders, where exact numbers and volumes are determined by the configuration:

* Buy orders at price:
  * `M * (1 - δ)`
  * `M * (1 - δ - δ / 2)`
  * `M * (1 - δ - δ / 2 - δ / 2)`
  * And so on, where `n`th buy order's price is given by `M * (1 - δ - (n - 1) * δ / 2)`.
* Sell orders at price:
  * `M * (1 + δ)`
  * `M * (1 + δ + δ / 2)`
  * `M * (1 + δ + δ / 2 + δ / 2)`
  * And so on, where the `n`th sell order's price is given by `M * (1 + δ + (n - 1) * δ / 2)`.

If the market price has drifted way higher (_"way higher"_ as directed by the configuration) than the price at which buy orders were placed, buy orders would be canceled. Likewise, if the price has drifted way lower than the price at which sell orders were placed, those sell orders would be canceled.

## Running the market maker bot: System requirements

Minimum System Requirements:
- Memory: 500 MB
- CPU: 0.5vCPU (2.25 GHz CPU Base Frequency)
- Reliable and fast internet connection

Recommended System Requirements:
- Memory: 1GB
- CPU: 1vCPU (2.25 GHz CPU Base Frequency)
- Reliable and blasingly fast internet connection

The Market Maker Bot does not require much resources. If you choose to use the Kupo provider and run the Cardano Node yourself, that needs much more resources, especially on the Cardano Mainnet.

For the exact requirements please see current Cardano Node documentation

## Running the market maker bot: Using docker compose (simple)

The simplest way to start an MM bot instance is by using Docker compose.

After cloning the repository  only a few environment variables must be set. As soon as this has been done; the Market Maker can be started using `docker compose`:

``` bash
# Clone the repository:
git clone git@github.com:geniusyield/market-maker.git
cd market-maker
cp ./.env.template ./.env

# Update the .env files with your own values (including seed phrase and other configuration values):
nano .env

# Update the docker images:
docker compose pull
# Start the MM bot with your config:
docker compose up -d
# Check the logs:
docker compose logs mm
```

The variables must be specified in the `.env` file before calling `docker compose up -d`:
- `MAESTRO_API_KEY`: The MAINNET API key to be used for accessing the Maestro services.
- `SEED_PHASE`: The seed phrase to be used.
- `COLLATERAL_UTXO`: A suitable UTxO with 5 ADA to be used as collateral UTxO. (e.g.: `d235edd34566a425668a4751233dfc2c1cs23b11287340b202c35093433491df#0)
- `STAKE_ADDRESS`: The stake address to be used. (e.g.: `stake1uhpdg7r5hjw5u2c59vtdgn3hmcjttlf8hdr5lx7ca5ut6rscues4h)

For example values plase see the `.env.template` file.

The configuration values used for these environment variables in the example above are just placeholders. These must be replaced by your own
configuration values. A MAINNET Maestro API key, a seed phrase and aa collateral UTxO must be provided after
sending funds to the address given by the payment signing key and the (optional) stake address.

In order to determine this address, you could use `cardano-cli address build`, but you can also just run the market maker - the address will be printed as the first log of "Info" severity:

```
Genius Yield Market Maker: <MARKET MAKER ADDRESS>
```

Maestro API keys are available after registration via the following link:
 - https://docs.gomaestro.org/Home/Getting-started/Sign-up-login

> [!WARNING]
> Please make sure to adapt the `MARKET_MAKER_CONFIG` configuration according to your needs! Please see the docker-compose.yml file for further details. For the configuration of the Market Maker, please see the [Configuration Settings](#Configuration) chapter.

## Running the market maker bot: Building from source (advanced)

In case you would like to build the Market Maker Bot from source, this chapter covers how to accomplish this.

> [!TIP]
> If you are not planning to contribute to the project, simply using the pre-built docker image, as described above, is likely the easier way to get started.

First, you need to setup the necessary tooling to work with [haskell.nix](https://github.com/input-output-hk/haskell.nix), then simply run `nix develop`, and it will drop you into a shell with all the necessary tools. Once inside the environment, you can build the order bot with `cabal build all`.

> [!NOTE]
> Nix is not necessary if your environment already has the right set of dependencies. One may look at the [CI file](https://github.com/geniusyield/atlas/blob/main/.github/workflows/haskell.yml) for our transaction building tool, which current project also relies on, to see dependencies used.

Then the bot can be ran with following command: `cabal run geniusyield-market-maker-exe -- Run my-atlas-config.json my-maker-bot-config.yaml` where `my-atlas-config.json` is the configuration for [Atlas](https://github.com/geniusyield/atlas) and `my-maker-bot-config.yaml` is the configuration of our market maker bot.

See [`atlas-config-maestro.json`](./atlas-config-maestro.json) & [`atlas-config-kupo.json`](./atlas-config-kupo.json) as an example of Atlas configuration using [Maestro](https://www.gomaestro.org/) provider & local node with [Kupo](https://github.com/CardanoSolutions/kupo) respectively.

### Configuration

The Market Maker Bot configuration had been explained in detail in the recent workshop, that is available on demand on YouTube:
 - [How to Create Your Own Market Maker Bot | Workshop with Dr. Lars Brünjes and the Genius Yield Team](https://www.youtube.com/watch?v=OCXVMmOB108&t=2391s)

> [!NOTE]
> See [`sample-preprod-maker-bot-config-gens.yaml`](./sample-preprod-maker-bot-config-gens.yaml) and [`sample-mainnet-maker-bot-config-gens.yaml`](./sample-mainnet-maker-bot-config-gens.yaml) for sample Preprod and Mainnet market maker bot configuration respectively. Note that both `json` and `yaml` formats are supported.

https://github.com/geniusyield/market-maker/blob/1c71faba6ff54c62cc8100eecbae69c537f058c4/sample-preprod-maker-bot-config-gens.yaml#L1-L93

* `mbc_user` describes bot's wallet.
  * `ur_stake_address` (optional) is the bech32 stake address (`stake_test1...` for testnet and `stake1...` for mainnet). If specified, bot would place orders at the mangled address so that ADA in those orders (both as an offer or as received payment) would be staked. Note that if an order undergoes partial fill, received payment is in the generated order UTxO and is received by the author of order only when order is completely filled or is cancelled.
  * `ur_s_key_path` (optional) is the path to payment signing key file (normal or extended). Note that we compute address of the bot (where funds should be provided) using payment key hash from this key and stake key hash from `ur_stake_address` (if provided).
  * `ur_mnemonic` (optional) is the mnemonic seed phrase to load the wallet. Either one of `ur_s_key_path` or `ur_mnemonic` must be provided.
    Specifying `acc_ix` and `addr_ix` is optional and if not provided, default value of zero is used. `acc_ix` specifies account index and `addr_ix` is used to specify address index to derive for payment key. Explicitly, such a key would have payment derivation hierarchy as `1852H/1815H/acc_ixH/0/addr_ix` and stake derivation hierarchy as `1852H/1815H/acc_ixH/2/0`[^fun]. In case you don't know what account index and address index mean in this context, you are likely well off omitting these fields. Note that in case `ur_stake_address` is also provided then it is instead used to determine for stake credential component of the bot's address instead of stake key hash obtained from above stake key derivation. Payment credential of bot's address is always obtained from above payment key derivation.

> [!TIP]
> Sample mnemonic provided above is a valid one and can be used to toy around with configuration to understand implications better.

Note that two prices providers are supported:  *Maestro* and *Taptools*.  It is optional to use only one or both.  In the latter case, MMBot will temporarily exit the market if a discrepancy beyond certain threshold arises between both prices providers.

  * `ur_coll` (optional) is the UTxO to be reserved as collateral. Though specifying `ur_coll` is optional but it is advised to set it as then this UTxO would be reserved (i.e., would not be spent) and thus be always available to serve as collateral. It is preferred for `ur_coll` to be pure 5 ADA only UTxO (i.e., no other tokens besides ADA).
* `mbc_delay` - Bot in single iteration tries to determine which orders need to be placed and which are needed to be cancelled. Once determined, it tries building the transactions and proceeds with submitting them, completing this single iteration. `mbc_delay` determines time in microseconds that bot must wait before proceeding with next iteration.
* `mbc_price_config` gives the configuration on how to get market price using [Maestro](https://docs.gomaestro.org/Cardano/DefiMarketAPI/mkt-dex-ohlc) or [Taptools](https://openapi.taptools.io/#tag/Market-Tokens/paths/~1token~1prices/post) endpoints, for a token.
  * `pc_price_common_cfg` contains the configuration parameters common to both prices providers.
    * `pcc_network_id` determines Cardano network which is mentioned for in API calls. It should always be kept `mainnet` as of now.
    * `pcc_price_diff_threshold1` if the *relative standard deviation*[^relstddev] among the prices providers is above this parameter, automatic cancelation of open orders is triggered (without raising logs' severity to "warning").
    * `pcc_price_diff_threshold2` which must be greater than or equal to the above, raises logs' severity to "warning".
    * `pcc_after_exit_relax_aim1` is the relaxation waiting time (in iterations) before re-entering the market (resume strategy) after piercing threshold1.
    * `pcc_after_exit_worse_max1` is the waiting time (in iterations) to raise logs' severity to "warning" if mismatch among prices providers persists.
    * `pcc_after_exit_relax_aim2` is the relaxation waiting time (in iterations) before re-entering the market after piercing threshold2.
    * `pcc_prices_providers_weights` is the list of relative weights for each prices provider in the computation of the average price.
  * `pc_prices_provider_cfgs` contains the configuration parameters for each prices provider.

  * Maestro:
    * `mc_api_key` is the Maestro API key.
    * `mc_resolution` is the resolution for the mentioned Maestro endpoint. Please see documentation [here](https://docs.gomaestro.org/Cardano/DefiMarketAPI/Introduction#prices) on how resolution helps determine price. Possible values of resolution can be seen [here](https://docs.gomaestro.org/Cardano/DefiMarketAPI/mkt-dex-ohlc). We take the closing price of the latest resolution window.
    * `mc_dex` determines DEX from which market price is queried for. Currently `minswap` & `genius-yield` are supported. Caution must be exercised in setting this value. We use the closing price from Maestro's OHLC endpoint and a price feed from AMM dex is less susceptible to price alterations as trades cannot happen at an arbitrary price.
    > [!CAUTION]
    > Please make sure to use `minswap` for the `pc_dex` configuration setting. Using an AMM based DEX as price oracle helps to combat malicious price manipulation.

     * `mc_pair_override` is optional and is needed in case one is not running bot on Mainnet. Since tokens on test network aren't actively traded, their price is not returned for by Maestro endpoint. To still get mainnet price for a corresponding mainnet token, one can specify desired (overriding) pair in `mpo_pair` & mention whether commodity is first token of the given pair or not in `mpo_commodity_is_first` field. In the above configuration, we are overriding the testnet GENS asset class `c6e65ba7878b2f8ea0ad39287d3e2fd256dc5c4160fc19bdf4c4d87e.7447454e53`, for the mainnet token pair `ADA-GENS`, and GENS is the second token in the pair so `mpo_commodity_is_first` is set to **false**. If the pair instead was `GENS-ADA` then `mpo_commodity_is_first` should be set to **true**.

  * Taptools:
    * `ttc_api_key` is the Taptools API key.
    * `ttc_pair_override` plays the same role as `mc_pair_override` described above, but for Taptools.  Note that asset class is set with `ttpo_asset` and its precison (number of decimals) with `ttpo_precision`.

* `mbc_strategy_config` determines parameters for strategy:

  * `sc_spread` - Ratio representing `δ` as described before. _[Since version 0.3.0]_ Note that one can optionally specify different `δ` to be used for buy side and sell side as shown in sample configuration, otherwise the same `δ` is used for both sides. Let buy side ratio be denoted by `δb` and sell side by `δs`, then `n`th buy order is placed at price `M * (1 - δb - (n - 1) * δb / 2)`, whereas `n`th sell order is placed at price `M * (1 + δs + (n - 1) * δs / 2)`.
  * `sc_cancel_threshold_product` - If the price in buy order is less than `(1 - sc_cancel_threshold_product * δb) * M`, then it is canceled. Likewise if the price in sell order is greater than `(1 + sc_cancel_threshold_product * δs) * M` then it is canceled.
  * _[Since version 0.3.0]_ `sc_preemptive_cancel_spread_ratio` (optional) - We aim to cancel those orders for which market price has "crossed" them where "crossing" is defined as follows: If price of created sell order is denoted by `ps` and current market price by `cp`, then we'll cancel this sell order if `ps <= cp * (1 + δs * sc_preemptive_cancel_spread_ratio)`. Likewise, if price of buy order is denoted by `pb`, then it will be cancelled if `pb >= cp * (1 - δb * sc_preemptive_cancel_spread_ratio)`. Value provided for `sc_preemptive_cancel_spread_ratio` must be b/w 0 & 1 (inclusive) and if not provided, it is assumed to be zero.
  * `sc_token_volume` specifies following:
    * `tv_sell_min_vol` - Amount of commodity tokens (in lowest possible denomination) that order must at least offer.
    * `tv_buy_min_vol` - Amount of currency tokens (in lovelaces) that order must at least offer.
    * `tv_sell_budget` - Total amount of commodity tokens that bot can cumulatively offer in the orders. In every iteration, bot determines the number of commodity tokens locked in the orders and subtracts it from `tv_sell_budget` field, let's call the obtained number `asb` (short for _available sell budget_) then it determines number of sell orders placed to be `⌊asb / tv_sell_min_vol⌋ = ns` where `ns` is short of number of sell orders. Now bot would place `ns` sell orders, each having offer amount as `⌊asb / ns⌋`.
    * `tv_buy_budget` - Total amount of currency tokens that bot can cumulatively offer in the orders. It governs bot symmetric to `tv_sell_budget`.
    * `tv_sell_vol_threshold` - this is related to `sc_price_check_product`. Bot would build an order book from all the orders for the given pair in GeniusYield DEX. It will sum the offered commodity tokens for sell orders which have price less than `M * (1 + sc_price_check_product * δs)` to get `SV` (short for sell volume) and sum the asked commodity tokens for buy orders which have price greater than `M * (1 - sc_price_check_product * δb)` to get `BV'` (short for buy volume in commodity token). We'll multiply `BV'` with `M` to get `BV` to represent buy volume in currency token. Now, bot would not place a new sell order, if `tv_sell_vol_threshold` is less than or equal to `SV`. Idea is that if there is enough liquidity then bot need not place orders. Symmetrically, bot would not place new buy orders only if `tv_buy_vol_threshold` is less than or equal to `BV`.
* `mbc_token` specifies the commodity token with it's precision. Note that this must not be ADA!

## Canceling all the orders using docker (simple)

If you would like to cancel *ALL* orders placed by your Market Maker Instance ran in Docker, you can do this by executing the following commands:

``` bash
# Clone the repository:
git clone git@github.com:geniusyield/market-maker.git
cd market-maker
# Stop the market maker (in case it is still running):
docker compose down
# TODO: update the following values with your own configuration:
export MAESTRO_API_KEY=aBcDefghijoXj3v0LB3txySofSPrP3Vf2
export PAYMENT_SIGNING_KEY='{ "type": "PaymentSigningKeyShelley_ed25519", "description": "Payment Signing Key", "cborHex": "4210268dsb870d08s83a4cf6a4408240248ea551a35bb22bf443586c233ae56bc340" }'
export COLLATERAL_UTXO=d235edd34566a425668a4751233dfc2c1cs23b11287340b202c35093433491df#0
export MODE=CANCEL
# Update the docker images:
docker compose pull
# Start the MM bot in 'CANCEL' mode:
docker compose up
```

You should see log entries with `X orders to cancel!` and finally `No more orders to cancel!` messages after all the orders placed by your MM instance had been canceled.

The final `ExitSuccess` and the `mm exited with code 0` output confirms that all went well.

## Canceling all the orders using cabal (advanced)

If you would like to cancel *ALL* orders placed by your Market Maker Instance and you built it from source, you can simply run:

``` bash
cabal run geniusyield-market-maker-exe -- Cancel my-atlas-config.json my-maker-bot-config.yaml
```

The output should be similar like in the previous chapter.

## Operational Costs

Here we try to list costs which market maker incurs when interacting with our DEX which would help in better decision for configuration values such as _spread_.

### Order placement

Order placement incurs following fees besides usual transaction fees.

* Flat fees: Every order is charged 1 ADA flat maker fee on creation but order author will get this back only if order underwent no partial filling.
* Percent fees: Every order is charged 0.3% of offered tokens on creation. If an order is cancelled afterwards, 0.3% percent would be charged only on the amount which actually got filled and remaining is refunded. As an example, suppose an order is created - offering 100 GENS. 0.3% of it is 0.3 GENS, which is initially charged. Now if this order is cancelled after only 60 GENS from it was consumed, then order author would get back 0.3% of 40 GENS namely, 0.12 GENS.

### Order cancellation

_tl;dr_ We group up to 6 order cancellations in a single transaction, fees incurred is usual transaction fee plus additional ADA up to 0.5, in worst case.

Order cancellation is slightly complex.

* Order underwent no fills: Only the usual network transaction fee is charged.
* Order underwent some filling: In this case, ADA taker fee might be added to this order or not. If it is added, only the usual network transaction fee is charged. However, if it is not added then as cancelling this order would require a fee output to GeniusYield address be generated, minimum ADA requirement of this fee output must be satisfied which currently stands in worst case at slightly less than 1.5 ADA. Now since maker certainly added 1 ADA due to flat ADA maker fee, it in worst case, would need to put additional 0.5 ADA. Note that we split orders to be cancelled in set of size 6 and then submit cancellation transaction for each of these sets. Thus if there are 6 orders to be cancelled for in a single set, then this additional 0.5 ADA, if needed, is shared across these 6 orders as fee output is to be generated once per transaction and not once per order. As a further illustration, if the bot had 13 orders to cancel, we will generate 3 sets of sizes 6, 6 & 1 and thus submit 3 cancellation transactions.

### Equity monitoring

Bot repeatedly logs for "normalized equity" in terms of ADA where ADA equivalent of commodity token is obtained by using price provider. As an example, if wallet has 500 ADA and 500 GENS and if price of 1 GENS is 2 ADA, then equity of wallet would be 1500 ADA. Besides normalized version, bot also logs for equity in terms of inventory balance both for open orders and user's wallet.

## Yield Accelerator Rewards

Market Maker Bots place orders and therefore participate in the GeniusYield Yield Accelerator Program and can accumulate rewards.

Traders wishing to check and claim their rewards can easily do so in the [GeniusYield UI](https://app.geniusyield.co/earn),
but unfortunately, at the moment, the UI only works for users who connect their wallets to the UI and are identified by
the wallet stake key hash.

If a bot operator uses a wallet stake key hash for the bot, the operator can use the UI to check and claim bot rewards.

If, on the other hand, the bot just uses a simple payment signing key and an associated address without staking component,
the UI cannot be used to check and claim rewards. In order to allow bot operators to check and claim rewards in this case,
we are providing two simple bash scripts, 
which can be found in the [SOR repository](https://github.com/geniusyield/smart-order-router?tab=readme-ov-file#yield-accelerator-rewards).
Both scripts require the `cardano-cli` to be installed and available in the `PATH`, and in order to claim,
you additionally need a connection to a running Cardano node.

## License

[Apache-2.0](./LICENSE) © [GYELD GMBH](https://www.geniusyield.co).


[^1]: _Display unit_ is one to which decimals are added as directed under [`cardano-token-registry`](https://github.com/cardano-foundation/cardano-token-registry).
[^fun]: Fun fact: Ada Lovelace lived from 1815 to 1852 which corresponds to numbers (namely _coin type_ & _purpose_) given in the hierarchy path.
[^relstddev]: The *relative standard deviation* of two prices `p1` and `p2` is `abs(p1 - p2) / (p1 + p2)`.

