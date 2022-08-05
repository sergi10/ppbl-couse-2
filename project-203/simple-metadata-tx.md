## 203.2: How to Include Metadata in a Cardano Transaction
If you know how to send a simple transaction with `cardano-cli`, then there is just one more thing you need to know in order to include metadata in your transaction.

In this example, notice that is no `--tx-out`. You will simply send a transaction to yourself. Adding addition `--tx-in`'s or `--tx-out`'s will have no effect on the way we include metadata in the transaction.

### Prepare metadata JSON file:
- Create a new `.json` file like `simple-metadata.json` (in `/project-203/metadata`)
- Here is a minimal example of what you can include in the file:
```
{
    "1337": "greetings from ppbl summer 2022"
}
```

### Set Variables
```
TXIN=""
SENDERADDRESS=""
SENDERKEY="path/to/payment.skey"
METADATA_JSON_FILE="/path/to/simple-metadata.json"
```

### Build Transaction
```
cardano-cli transaction build \
--babbage-era \
--testnet-magic 1097911063 \
--tx-in $TXIN \
--change-address $SENDERADDRESS \
--metadata-json-file $METADATA_JSON_FILE \
--protocol-params-file protocol.json \
--out-file tx-with-metadata.raw

cardano-cli transaction sign \
--signing-key-file $SENDERKEY \
--testnet-magic 1097911063 \
--tx-body-file tx-with-metadata.raw \
--out-file tx-with-metadata.signed

cardano-cli transaction submit \
--tx-file tx-with-metadata.signed \
--testnet-magic 1097911063
```

### Search for TxHash on CardanoScan
[https://testnet.cardanoscan.io](https://testnet.cardanoscan.io)