# Project: Build a Faucet

## Part 3: Test Locking and Unlocking Transactions with `cardano-cli`
### Updated 2022-10-01 with new Example Contract Address

Try to unlock tokens from the contract at: `addr_test1wrfp7hgj52px3a6lwq7sed2peqfe476sx3n2ceg0xx0vpmc2myjrk`. This contract is configured to allow users to withdraw 175 `tgimbal` tokens per withdrawal (ie "unlocking transaction").

### Step By Step:
1. Get the `ppbl-pre-prod-faucet-tgimbal-175.plutus` file provided here in `/project-301-faucet/shared-script/`
2. Create a `redeemer.json` file with the following contents:
```
{"constructor":0,"fields":[{"bytes":"<YOUR PUBKEYHASH HERE"}]}
```
3. Note the Datum and DatumHash:
```
cardano-cli transaction hash-script-data --script-data-value 1618
> 2da1c63e7646ce8cc514113c66e9cefb79e482210ad1dadb51c2a17ab14cf114
```
4. Build an Unlocking Transaction to get some `tgimbal` tokens:
#### Set Variables
```
CONTRACT_TXIN=""
AUTH_TOKEN_TXIN=""
FEE_TXIN=""
COLLATERAL=""
PLUTUS_SCRIPT_FILE="<YOUR PATH TO>/ppbl-course-02/project-301-faucet/output/ppbl-faucet-preprod-tgimbal-175.plutus"
ASSET="fb45417ab92a155da3b31a8928c873eb9fd36c62184c736f189d334c.7467696d62616c"
AUTH_TOKEN="748ee66265a1853c6f068f86622e36b0dda8edfa69c689a7dd232c60.5050424c53756d6d657232303232"
TOKENS_BACK_TO_CONTRACT=<will be the number of token in the contract, minus 3000 to be withdrawn>
CONTRACTADDR="addr_test1wrfp7hgj52px3a6lwq7sed2peqfe476sx3n2ceg0xx0vpmc2myjrk"
DATUMHASH="2da1c63e7646ce8cc514113c66e9cefb79e482210ad1dadb51c2a17ab14cf114"
```
Note: `$ASSET` represents `tgimbal`; `$AUTH_TOKEN` represents `PPBLSummer2022`

#### Build Unlocking Transaction
```
cardano-cli transaction build \
--babbage-era \
--testnet-magic 1 \
--tx-in $AUTH_TOKEN_TXIN \
--tx-in $FEE_TXIN \
--tx-in $CONTRACT_TXIN \
--tx-in-script-file $PLUTUS_SCRIPT_FILE \
--tx-in-datum-value 1618 \
--tx-in-redeemer-file redeemer.json \
--tx-in-collateral $COLLATERAL \
--tx-out $SENDER+"2000000 + 3000 $ASSET" \
--tx-out $SENDER+"2000000 + 1 $AUTH_TOKEN" \
--tx-out $CONTRACTADDR+"2000000 + $TOKENS_BACK_TO_CONTRACT $ASSET" \
--tx-out-datum-hash $DATUMHASH \
--change-address $SENDER \
--protocol-params-file protocol.json \
--out-file unlock.raw

cardano-cli transaction sign \
--signing-key-file $SENDERKEY \
--testnet-magic 1 \
--tx-body-file unlock.raw \
--out-file unlock.signed

cardano-cli transaction submit \
--tx-file unlock.signed \
--testnet-magic 1
```

# Create Your Own Faucet:

### Compile `ppbl-faucet-<YOUR TOKEN>.plutus`
- Follow the steps in `/project-301-faucet/src/FaucetValidatorScript` to compile your custom contract script.

### Build Address
```
cardano-cli address build \
--payment-script-file ppbl-faucet-<YOUR TOKEN>.plutus \
--testnet-magic 1 \
--out-file ppbl-faucet-<YOUR TOKEN>.addr
```

### Prepare Datum
```
cardano-cli transaction hash-script-data --script-data-value <SOME NUMBER>
> <OUTPUT>
```

### 3d. Build and Submit Locking Transaction
Make sure that your wallet has some of your faucet token, and you may have to move your `PPBLSummer2022` token around a few times. You've got this!

Set Variables
```
SENDER
SENDERKEY
TXIN1="667392e97c858112dd71ae7eadb73cdb1831e2ac404afcac195563a8561dc9a4#2"
TXIN2="667392e97c858112dd71ae7eadb73cdb1831e2ac404afcac195563a8561dc9a4#0"
CONTRACTADDR="addr_test1wrfp7hgj52px3a6lwq7sed2peqfe476sx3n2ceg0xx0vpmc2myjrk"
DATUMHASH="2da1c63e7646ce8cc514113c66e9cefb79e482210ad1dadb51c2a17ab14cf114"
ASSET="fb45417ab92a155da3b31a8928c873eb9fd36c62184c736f189d334c.7467696d62616c"
NUM_TOKENS= how many tokens do you want to lock?
```

Build a Locking Transaction:
```
cardano-cli transaction build \
--babbage-era \
--tx-in $TXIN1 \
--tx-in $TXIN2 \
--tx-out $CONTRACTADDR+"2000000 + $NUM_TOKENS $ASSET" \
--tx-out-datum-hash $DATUMHASH \
--change-address $SENDER \
--protocol-params-file protocol-preprod.json \
--out-file tx-lock.raw \
--testnet-magic 1

cardano-cli transaction sign \
--signing-key-file $SENDERKEY \
--testnet-magic 1 \
--tx-body-file tx-lock.raw \
--out-file tx-lock.signed

cardano-cli transaction submit \
--tx-file tx-lock.signed \
--testnet-magic 1

```


### Extended Explorations:
1. There is (at least one) major flaw in our Plutus Faucet Validator as it is currently written. Think about the Plutus logic and the requirements of our unlocking transaction. What edge cases can you find?

2. It is possible to use the Redeemer to create a case where the maintainer of a Faucet can easily add additional tokens to the Contract UTxO. In this contract, there is no direct way to do so. Even so, we can build a transaction that provides this functionality. Here is an example:

```
cardano-cli transaction build \
--babbage-era \
--testnet-magic 1 \
--tx-in $AUTH_TOKEN_TXIN \
--tx-in $FEE_TXIN \
--tx-in $ADDITIONAL_TOKEN_TXIN \
--tx-in $CONTRACT_TXIN \
--tx-in-script-file $PLUTUS_SCRIPT_FILE \
--tx-in-datum-value 1618 \
--tx-in-redeemer-file redeemer.json \
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



## Send a bunch of PPBLSummer2022 Tokens on Pre-Prod
#### (You can use this as a template for additional access tokens)
RECEIVER1=addr_test1qq3c70lkdqtsg9he2ehlx45zje7d46wkc5jdxfhdmlea46mr4tdgk5rlhnujvt32c0hf89z7yndw9mdyglqslcuvr6rqqxfa7u
RECEIVER2=addr_test1qzj5ayxmzhdrwtwe06p27mw2p83jgysx0cgk6z29400vj02c5lx6fe7jrx9r97qhuyt7gqv8ytea96ugq6sfxmrwwjhqwskj0g
RECEIVER3=addr_test1qrasyvgzq036n599efyxnzwnmvnxhc2pfhgwk3przltm724pqcrstmq0m7rp27678g76yu0p99f76kvy3w668su6ua6q5gdd00
RECEIVER4=addr_test1qp0gyn7hp4cmhpegywqffyrqmp9hxjuc03zs4qj5ykfey47h3xqep2a6y20j6wpdmccj09l5v5x0r8mnv5020wfxnzssafmm5t
RECEIVER5=addr_test1qrh56dw8e6ms8hlv4p5rpn0ujg97fjngj5p2e624fwdyttf4smlkwuv0fa569kt0sejlfeq8fkhps8f6dr6m9at6wx3q302ef0

cardano-cli transaction build \
--babbage-era \
--tx-in $TXIN1 \
--tx-in $TXIN2 \
--tx-out $RECEIVER1+"2000000 + 1 748ee66265a1853c6f068f86622e36b0dda8edfa69c689a7dd232c60.5050424c53756d6d657232303232" \
--tx-out $RECEIVER2+"2000000 + 1 748ee66265a1853c6f068f86622e36b0dda8edfa69c689a7dd232c60.5050424c53756d6d657232303232" \
--tx-out $RECEIVER3+"2000000 + 1 748ee66265a1853c6f068f86622e36b0dda8edfa69c689a7dd232c60.5050424c53756d6d657232303232" \
--tx-out $RECEIVER4+"2000000 + 1 748ee66265a1853c6f068f86622e36b0dda8edfa69c689a7dd232c60.5050424c53756d6d657232303232" \
--tx-out $RECEIVER5+"2000000 + 1 748ee66265a1853c6f068f86622e36b0dda8edfa69c689a7dd232c60.5050424c53756d6d657232303232" \
--tx-out $WALLET1+"2000000 + 130 748ee66265a1853c6f068f86622e36b0dda8edfa69c689a7dd232c60.5050424c53756d6d657232303232" \
--change-address $WALLET1 \
--protocol-params-file protocol-preprod.json \
--out-file tx-lock.raw \
--testnet-magic 1
