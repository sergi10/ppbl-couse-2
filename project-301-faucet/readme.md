# Project 301: Build a Faucet

## 301 - Mastery Level 1: Unlock tgimbals from Faucet on Pre-Production

### Updated 2022-10-22 with new Example Contract Address for NEW Pre-Production
Note: Cardano Pre-Production Testnet was "respun" on 2022-10-21

Try to unlock tokens from the contract at: `addr_test1wqunsl063ezhn67r72uxetvjugrmnnrkqu8pmd9exc6lcdcgnt2uf`. This contract is configured to allow users to withdraw 1000 `tGimbal` tokens per withdrawal (ie "unlocking transaction").

### Step By Step:
1. Get the `ppbl-faucet-preprod-2022-oct-tgimbal.plutus` file provided here in `/project-301-faucet/shared-script/`
2. Create a `redeemer.json` file with the following contents:
```
{"constructor":0,"fields":[{"bytes":"<YOUR PUBKEYHASH HERE"}]}
```
3. Note the Datum and DatumHash:
```
cardano-cli transaction hash-script-data --script-data-value 1618
> 2da1c63e7646ce8cc514113c66e9cefb79e482210ad1dadb51c2a17ab14cf114
```
4. Build an Unlocking Transaction to get some `tGimbal` tokens:

#### 4a. Set Variables
```
SENDER=
SENDERKEY=
CONTRACT_TXIN=
AUTH_TOKEN_TXIN=
FEE_TXIN=
COLLATERAL=
PLUTUS_SCRIPT_FILE="<YOUR PATH TO>/ppbl-course-02/project-301-faucet/output/ppbl-faucet-preprod-2022-oct-tgimbal.plutus"
ASSET="fb45417ab92a155da3b31a8928c873eb9fd36c62184c736f189d334c.7447696d62616c"
AUTH_TOKEN="748ee66265a1853c6f068f86622e36b0dda8edfa69c689a7dd232c60.5050424c32303232416363657373546f6b656e"
TOKENS_BACK_TO_CONTRACT=<NUMBER tGimbals IN CONTRACT> - 1000
CONTRACTADDR=addr_test1wqunsl063ezhn67r72uxetvjugrmnnrkqu8pmd9exc6lcdcgnt2uf
DATUMHASH=2da1c63e7646ce8cc514113c66e9cefb79e482210ad1dadb51c2a17ab14cf114
```
Note: `$ASSET` represents `tGimbal`; `$AUTH_TOKEN` represents `PPBL2022AccessToken`

#### 4b. Build Unlocking Transaction
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
--tx-out $SENDER+"1500000 + 1000 $ASSET" \
--tx-out $SENDER+"1500000 + 1 $AUTH_TOKEN" \
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

### Important - It's the Babbage Era!
Note the fee for this transaction. It should be a little more than 0.63 ADA, or ~630000 lovelace. That is expensive for a transaction! But remember that we are using a PlutusV1 Script. This is the "old" version of Plutus. You will learn how to use PlutusV2 Scripts - and see the cost savings - later in this course.

---

# 301 - Mastery Level 2: Create and Test Your Own Faucet Instance

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
Make sure that your wallet has some of your faucet token, and you may have to move your `PPBL2022AccessToken` token around a few times. You've got this!

Set Variables
```
SENDER
SENDERKEY
TXIN1=
TXIN2=
CONTRACTADDR=
DATUMHASH=
ASSET=
NUM_TOKENS=500000
```

Build a Locking Transaction:
```
cardano-cli transaction build \
--babbage-era \
--tx-in $TXIN1 \
--tx-in $TXIN2 \
--tx-out $CONTRACTADDR+"1500000 + $NUM_TOKENS $ASSET" \
--tx-out-datum-hash $DATUMHASH \
--change-address $SENDER \
--protocol-params-file protocol.json \
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


