# Project: Build a Faucet

## Part 3: Test Locking and Unlocking Transactions with `cardano-cli`

---

### 3a. Compile `ppbl-faucet-<YOUR TOKEN>.plutus`
- Follow the steps in `/project-301-faucet/src/FaucetValidatorScript` to compile your custom contract script.

### 3b. Build Address
```
cardano-cli address build \
--payment-script-file ppbl-faucet-<YOUR TOKEN>.plutus \
--testnet-magic 1097911063 \
--out-file ppbl-faucet-<YOUR TOKEN>.addr
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
--out-file tx-lock.raw \
--testnet-magic 1097911063

cardano-cli transaction sign \
--signing-key-file $SENDERKEY \
--testnet-magic 1097911063 \
--tx-body-file tx-lock.raw \
--out-file tx-lock.signed

cardano-cli transaction submit \
--tx-file tx-lock.signed \
--testnet-magic 1097911063

```

### 3e. Build and Submit Unlocking Transaction

Set Variables
```
CONTRACT_TXIN=""
AUTH_TOKEN_TXIN=""
FEE_TXIN=""
PLUTUS_SCRIPT_FILE="/path/to/ppbl-faucet-<YOUR TOKEN>.plutus"
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


### Extended Explorations:
1. There is (at least one) major flaw in our Plutus Faucet Validator as it is currently written. Think about the Plutus logic and the requirements of our unlocking transaction. What edge cases can you find?

2. It is possible to use the Redeemer to create a case where the maintainer of a Faucet can easily add additional tokens to the Contract UTxO. In this contract, there is no direct way to do so. Even so, we can build a transaction that provides this functionality. Here is an example:

```
cardano-cli transaction build \
--babbage-era \
--testnet-magic 1097911063 \
--tx-in $AUTH_TOKEN_TXIN \
--tx-in $FEE_TXIN \
--tx-in $ADDITIONAL_TOKEN_TXIN \
--tx-in $CONTRACT_TXIN \
--tx-in-script-file $PLUTUS_SCRIPT_FILE \
--tx-in-datum-value 1618 \
--tx-in-redeemer-value 101 \
--tx-in-collateral $COLLATERAL \
--tx-out $CONTRACT_ADDR+"2000000 + $TOTAL $ASSET" \
--tx-out-datum-hash $DATUMHASH \
--tx-out $SENDER+"2000000 + $REMAINING $ASSET" \
--tx-out $SENDER+"2000000 + 1 $AUTH_TOKEN" \
--change-address $SENDER \
--required-signer-hash $SENDER_PKH \
--protocol-params-file protocol.json \
--out-file unlock.raw
```

Try to define appropriate values for each of the variables in the Transaction template above. How would we calculate the value of `$TOTAL` in order make this transaction valid?

* Hint: look up this transaction in a Testnet explorer: `5a2f5f2d814757e641b30363a90bef742c5bef6f4ba7f13b387c2ac618d667a1`
