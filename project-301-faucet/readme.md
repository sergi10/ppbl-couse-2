# Project: Build a Faucet

## Part 3: Test Locking and Unlocking Transactions with `cardano-cli`

---

### 3a. Compile `ppbl-faucet-integer.plutus`
- In `ppbl-faucet.cabal`, look at Line 45, and make sure that `FaucetValidatorScriptInteger` is exported.
- In `FaucetValidatorScriptCompiler.hs`, look at Line 12, make sure that `FaucetValidatorScriptInteger` is imported.
- Follow the usual steps to compile a `.plutus` script (ie: run `cabal repl`, then run `writeFaucetScript`)

### 3b. Build Address
```
cardano-cli address build \
--payment-script-file ppbl-faucet-integer.plutus \
--testnet-magic 1097911063 \
--out-file ppbl-faucet-integer.addr
```

### 3c. Prepare Datum
```
cardano-cli transaction hash-script-data --script-data-value 1618
> 2da1c63e7646ce8cc514113c66e9cefb79e482210ad1dadb51c2a17ab14cf114
```

### 3d. Build and Submit Locking Transaction
Make sure that your wallet has some of your faucet token, and you may have to move your `PPBLSummer2022` token around a few times. You've got this!

Set Variables
```
SENDER
SENDERKEY
TXIN1=""
CONTRACTADDR=""
DATUMHASH="2da1c63e7646ce8cc514113c66e9cefb79e482210ad1dadb51c2a17ab14cf114"
ASSET=""

```

Build Locking Transaction
```
cardano-cli transaction build \
--babbage-era \
--tx-in $TXIN1 \
--tx-in $TXIN2 \
--tx-out $CONTRACTADDR+"2000000 + 12000 $ASSET" \
--tx-out-datum-hash $DATUMHASH \
--tx-out $SENDER+"2000000 + 488000 $ASSET" \
--change-address $SENDER \
--protocol-params-file protocol.json \
--out-file tx-lock-integer.raw \
--testnet-magic 1097911063

cardano-cli transaction sign \
--signing-key-file $SENDERKEY \
--testnet-magic 1097911063 \
--tx-body-file tx-lock-integer.raw \
--out-file tx-lock-integer.signed

cardano-cli transaction submit \
--tx-file tx-lock-integer.signed \
--testnet-magic 1097911063

```

### 3e. Build and Submit Unlocking Transaction

Set Variables
```
CONTRACT_TXIN=""
AUTH_TOKEN_TXIN=""
FEE_TXIN=""
PLUTUS_SCRIPT_FILE="/path/to/ppbl-faucet-integer.plutus"
COLLATERAL=""
AUTH_TOKEN="1309921891e459c7e9acb338d5dae18f98d1c2f55c1852cd5cf341f9.5050424c53756d6d657232303232"
SENDER_PKH=""
```

Build Unlocking Transaction
```
cardano-cli transaction build \
--babbage-era \
--testnet-magic 1097911063 \
--tx-in $AUTH_TOKEN_TXIN \
--tx-in $FEE_TXIN \
--tx-in $CONTRACT_TXIN \
--tx-in-script-file $PLUTUS_SCRIPT_FILE \
--tx-in-datum-value 1618 \
--tx-in-redeemer-value 101 \
--tx-in-collateral $COLLATERAL \
--tx-out $SENDER+"2000000 + 3000 $ASSET" \
--tx-out $SENDER+"2000000 + 1 $AUTH_TOKEN" \
--change-address $SENDER \
--required-signer-hash $SENDER_PKH \
--protocol-params-file protocol.json \
--out-file unlock.raw

cardano-cli transaction sign \
--signing-key-file $SENDERKEY \
--testnet-magic 1097911063 \
--tx-body-file unlock.raw \
--out-file unlock.signed

cardano-cli transaction submit \
--tx-file unlock.signed \
--testnet-magic 1097911063
```
