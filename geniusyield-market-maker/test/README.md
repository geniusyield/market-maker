# Test Suites

Test suites for market maker bot.

## pproviders-status-sequence

This suite tests a particular sequence of events involving a pair of mock prices providers.

### Sequence and validation

This sequence of events is defined by `testSequence :: [PPStatus]` in `test/GeniusYield/Test/MarketMaker/Utils.hs`.  The validation requirements are given by `testRequirements :: [LoggedEvent]` in the same file.  *Note:*  this particular test assumes that initial state of MMBot is without orders.

Both `testSequence` and `testRequirements` can of course be modified to test other scenarios.

### Environment variables

The following environment variables must be defined:

- TEST_FRAMEWORK_CONFIG_FILE : Atlas framework configuration file path.
- TEST_MMBOT_CONFIG_FILE : MMBot configuration file path.

(File paths are relative to directory `test/..` .)