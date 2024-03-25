# Revision history for geniusyield-market-maker

## 0.3.0 -- 2024-02-23

* `sc_preemptive_cancel_spread_ratio` replaces `sc_cancel_window_ratio`, for more information about it, see documentation.

## 0.2.1 -- 2024-02-22

* Added `sc_cancel_window_ratio` parameter to govern which orders are cancelled and changed the behavior introduced by version `0.2.0` where instead of cancelling all orders, we cancel only those for which price have crossed order's price within `sc_cancel_window_ratio`.

## 0.2.0 -- 2024-02-20

* Added a condition where bot would cancel all it's orders in case market price reaches or crosses price of any of it's placed order(s).
