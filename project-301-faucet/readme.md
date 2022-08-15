# Project: Build a Faucet

0. Prep Tokens
1. Get Started with Transactions in Front End Template
2. SC Interactions with `cardano-cli`
3. SC Transactions Front End
4. Your Task
5. Register Your Result with Metadata

---

# 2. SC Interactions with `cardano-cli`: Two Working Contracts
- `FaucetValidatorScriptInteger.hs`
- `FaucetValidatorScriptUnit.hs`

Here is how to test with each.

# Test With "Integer" Contract

### 2a. Compile `ppbl-faucet-integer.plutus`
- In `ppbl-faucet.cabal`, look at Line 45, and make sure that `FaucetValidatorScriptInteger` is exported.
- In `FaucetValidatorScriptCompiler.hs`, look at Line 12, make sure that `FaucetValidatorScriptInteger` is imported.
- Follow the usual steps to compile a `.plutus` script (ie: run `cabal repl`, then run `writeFaucetScript`)

### 2b. Build Address
```
cardano-cli address build \
--payment-script-file ppbl-faucet-integer.plutus \
--testnet-magic 1097911063 \
--out-file ppbl-faucet-integer.addr
```

### 2c. Prepare Datum
```
cardano-cli transaction hash-script-data --script-data-value 1618
> 2da1c63e7646ce8cc514113c66e9cefb79e482210ad1dadb51c2a17ab14cf114
```

### 2d. Locking Transaction
Make sure that your wallet has some of your faucet token, and you may have to move your `PPBLSummer2022` token around a few times. You've got this!

Set Variables
```
SENDER
SENDERKEY
TXIN1=""
CONTRACTADDR="addr_test1wrfc4hux6ffmqycnzzv6kfk2luqxeskgdn4m6l6z5txzcuqtw0r3d"
DATUMHASH="2da1c63e7646ce8cc514113c66e9cefb79e482210ad1dadb51c2a17ab14cf114"
ASSET="6c57132fde399c9ea6e462c4214d164984891087922b6fa2472b175b.7470626c5465737447696d62616c"

```

Build Locking Transaction
```
cardano-cli transaction build \
--babbage-era \
--tx-in $TXIN1 \
--tx-out $CONTRACTADDR+"5000000 + 10000 $ASSET" \
--tx-out-datum-hash $DATUMHASH \
--tx-out $SENDER+"5000000 + 10000 $ASSET" \
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

### 2e. Unlocking Transaction

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
--tx-out $CONTRACTADDR+"5000000 + 5000 $ASSET" \
--tx-out $SENDER+"2000000 + 5000 $ASSET" \
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

## Success!
https://testnet.cardanoscan.io/transaction/84d160c6d69a7d8d4f954034e8eac2b975e1414d7638fd80587f727ba89622af?tab=utxo

# Optional Extension: Test With "Unit" Contract

### 2a. Compile `ppbl-faucet-unit.plutus`
- In `ppbl-faucet.cabal`, look at Line 45, and change `FaucetValidatorScriptInteger` to `FaucetValidatorScriptUnit`
- In `FaucetValidatorScriptCompiler.hs`, look at Line 12, make sure that `FaucetValidatorScriptUnit` is imported.
- Then, change every occurence of `FaucetValidatorScriptInteger` to `FaucetValidatorScriptUnit` in `FaucetValidatorScriptCompiler.hs`
- Finally, change the file name of the `.plutus` script that will be written on line 19 to `ppbl-faucet-unit.plutus`
- Follow the usual steps to compile a `.plutus` script (ie: run `cabal repl`, then run `writeFaucetScript`)

### 2b. Build Address
```
cardano-cli address build \
--payment-script-file ppbl-faucet-unit.plutus \
--testnet-magic 1097911063 \
--out-file ppbl-faucet-unit.addr
```

### 2c. Prepare Datum
where `datum.json` is
```
{"constructor":0,"fields":[]}

```
```
cardano-cli transaction hash-script-data --script-data-file datum.json
> 923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec
```


### 2d. Locking Transaction
Make sure that your wallet has some of your faucet token, and you may have to move your `PPBLSummer2022` token around a few times. You've got this!

Set Variables
```
SENDER
SENDERKEY
TXIN1=""
TXIN2=""
CONTRACTADDR="addr_test1wrthd2qewg43c5wf64jwt0ud3fjr8txzgpwjrfvfmfrgayq38f9fr"
DATUMHASH="923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"
ASSET="6c57132fde399c9ea6e462c4214d164984891087922b6fa2472b175b.7470626c5465737447696d62616c"

```

Build Locking Transaction
```
cardano-cli transaction build \
--babbage-era \
--tx-in $TXIN1 \
--tx-in $TXIN2 \
--tx-out $CONTRACTADDR+"5000000 + 10000 $ASSET" \
--tx-out-datum-hash $DATUMHASH \
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

### 2e. Unlocking Transaction

Set Variables
```
CONTRACT_TXIN=""
AUTH_TOKEN_TXIN=""
FEE_TXIN=""
PLUTUS_SCRIPT_FILE="/path/to/ppbl-faucet-unit.plutus"
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
--tx-in-datum-file datum.json \
--tx-in-redeemer-file datum.json \
--tx-in-collateral $COLLATERAL \
--tx-out $CONTRACTADDR+"5000000 + 5000 $ASSET" \
--tx-out $SENDER+"2000000 + 5000 $ASSET" \
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

SUCCESS: https://testnet.cardanoscan.io/transaction/decdf14296fc39a1bb2ddac1f2a2a4932afdd5d15bf56dc4941e115b782ff44f
