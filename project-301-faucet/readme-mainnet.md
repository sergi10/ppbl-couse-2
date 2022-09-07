# Project: Build a Faucet

## Testing Faucet Contract on Mainnet (Danger!)

Try to unlock tokens from the contract at: `addr1w9tlx9ml6wc2eyuxhxaapn0wvs7z4qlzdqzqz8ljh5cq86q5atkch`

### Step By Step:
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
CONTRACT_TXIN="d06adf856deb43f61e9742a6a5d45a83b3d52225b1c5040e3883c6385ff55df7#1"
AUTH_TOKEN_TXIN="ec8680089c1a97dbb2d83caa1ff0e6afd065157c75b25d8fc58e724bb8203465#1"
FEE_TXIN="0f4b735021ecd7ffde11f9be83c0a4501dcb008ed97cb3a61ede26a693d287c0#0"
COLLATERAL="0f4b735021ecd7ffde11f9be83c0a4501dcb008ed97cb3a61ede26a693d287c0#2"
DATUMHASH="2da1c63e7646ce8cc514113c66e9cefb79e482210ad1dadb51c2a17ab14cf114"
PLUTUS_SCRIPT_FILE="/home/james/hd2/ppbl-course-02/ppbl-course-02/project-301-faucet/output/ppbl-faucet-mainnet-gimbal.plutus"
ASSET="2b0a04a7b60132b1805b296c7fcb3b217ff14413991bf76f72663c30.67696d62616c"
AUTH_TOKEN="28adc4b12edd23bad18823c0b0a74b24a95ccf45babf8a3782217f4f.5050424c436f6e747269624c6576656c31"
```
Note: `$ASSET` represents `gimbal`; `$AUTH_TOKEN` represents `PPBLContribLevel1`

#### Build Unlocking Transaction
```
cardano-cli transaction build \
--alonzo-era \
--mainnet \
--tx-in $AUTH_TOKEN_TXIN \
--tx-in $FEE_TXIN \
--tx-in $CONTRACT_TXIN \
--tx-in-script-file $PLUTUS_SCRIPT_FILE \
--tx-in-datum-value 1618 \
--tx-in-redeemer-file redeemer.json \
--tx-in-collateral $COLLATERAL \
--tx-out $RECEIVE+"2000000 + 25000000 $ASSET" \
--tx-out $RECEIVE+"2000000 + 1 $AUTH_TOKEN" \
--tx-out $CONTRACTADDR+"2000000 + 275000000 $ASSET" \
--tx-out-datum-hash $DATUMHASH \
--change-address $RECEIVE \
--protocol-params-file protocol-mainnet.json \
--out-file unlock.raw

cardano-cli transaction sign \
--signing-key-file $RECEIVEKEY \
--mainnet \
--tx-body-file unlock.raw \
--out-file unlock.signed

cardano-cli transaction submit \
--tx-file unlock.signed \
--mainnet
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
TXIN1="63e99dbb926b5a7e08adc7ef2f3710fc31ec26b2b15d3bf269885214e543cc8a#0"
TXIN2="ec8680089c1a97dbb2d83caa1ff0e6afd065157c75b25d8fc58e724bb8203465#0"
CONTRACTADDR="addr1w9tlx9ml6wc2eyuxhxaapn0wvs7z4qlzdqzqz8ljh5cq86q5atkch"
DATUMHASH="2da1c63e7646ce8cc514113c66e9cefb79e482210ad1dadb51c2a17ab14cf114"
ASSET="2b0a04a7b60132b1805b296c7fcb3b217ff14413991bf76f72663c30.67696d62616c"
PLUTUS_SCRIPT_FILE="/home/james/hd2/ppbl-course-02/ppbl-course-02/project-301-faucet/output/ppbl-faucet-mainnet-gimbal.plutus"

```

Build a Locking Transaction:
```
cardano-cli transaction build \
--alonzo-era \
--tx-in $TXIN1 \
--tx-in $TXIN2 \
--tx-out $CONTRACTADDR+"2000000 + 300000000 $ASSET" \
--tx-out-datum-hash $DATUMHASH \
--tx-out $SENDER+"2000000 + 700000000 $ASSET" \
--change-address $SENDER \
--protocol-params-file protocol-mainnet.json \
--out-file tx-lock.raw \
--mainnet

cardano-cli transaction sign \
--signing-key-file $SENDERKEY \
--mainnet \
--tx-body-file tx-lock.raw \
--out-file tx-lock.signed

cardano-cli transaction submit \
--tx-file tx-lock.signed \
--mainnet

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



## Send a bunch of PPBLSummer2022 Tokens on Pre-Prod
#### (You can use this as a template for additional access tokens)
RECEIVER1=addr_test1qq3c70lkdqtsg9he2ehlx45zje7d46wkc5jdxfhdmlea46mr4tdgk5rlhnujvt32c0hf89z7yndw9mdyglqslcuvr6rqqxfa7u
RECEIVER2=addr_test1qzj5ayxmzhdrwtwe06p27mw2p83jgysx0cgk6z29400vj02c5lx6fe7jrx9r97qhuyt7gqv8ytea96ugq6sfxmrwwjhqwskj0g
RECEIVER3=addr_test1qrasyvgzq036n599efyxnzwnmvnxhc2pfhgwk3przltm724pqcrstmq0m7rp27678g76yu0p99f76kvy3w668su6ua6q5gdd00
RECEIVER4=addr_test1qp0gyn7hp4cmhpegywqffyrqmp9hxjuc03zs4qj5ykfey47h3xqep2a6y20j6wpdmccj09l5v5x0r8mnv5020wfxnzssafmm5t
RECEIVER5=addr_test1qrh56dw8e6ms8hlv4p5rpn0ujg97fjngj5p2e624fwdyttf4smlkwuv0fa569kt0sejlfeq8fkhps8f6dr6m9at6wx3q302ef0

cardano-cli transaction build \
--alonzo-era \
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