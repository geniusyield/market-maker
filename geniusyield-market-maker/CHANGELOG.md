# Revision history for geniusyield-market-maker

## 0.4.0 -- 2024-05-02

* Supports latest version of [`dex-contracts-api`](https://github.com/geniusyield/dex-contracts-api) which adds support of v1.1 family of scripts.
* Updates to latest version of Maestro's Haskell SDK.

## 0.3.0 -- 2024-04-11

* Adds ability to specify different spread for each (sell & buy) side. The change is backwards compatible with respect to configuration.
* `sc_preemptive_cancel_spread_ratio` replaces `sc_cancel_window_ratio`, for more information about it, see documentation.

## 0.2.1 -- 2024-02-22

* Added `sc_cancel_window_ratio` parameter to govern which orders are cancelled and changed the behavior introduced by version `0.2.0` where instead of cancelling all orders, we cancel only those for which price have crossed order's price within `sc_cancel_window_ratio`.

## 0.2.0 -- 2024-02-20

* Added a condition where bot would cancel all it's orders in case market price reaches or crosses price of any of it's placed order(s).
