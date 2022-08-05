#  203.4 - Using CIP-25 to Mint a Cardano NFT

If you were able to mint a token with cardano-cli in **201.3 Minting Native Assets on cardano-cli** and to add metadata to a transaction in **203 Completion Assignment: Hello Testnet!**, then you've got the background knowledge necessary to mint a Cardano NFT.

## Review Cardano Improvement Proposal 25:
- [https://cips.cardano.org/cips/cip25/](https://cips.cardano.org/cips/cip25/)
- Use CIP-25 to create a `.json` file that adheres to the Cardano NFT Metatdata standard.

## Create a Policy ID
- see lesson [201.3](https://gitlab.com/gimbalabs/plutus-pbl-summer-2022/ppbl-course-02/-/blob/master/project-02/docs/201-3.md)
- Extension: [You can use scripts to create policies with additional parameters](https://github.com/input-output-hk/cardano-node/blob/master/doc/reference/simple-scripts.md).

## Set Variables
- Remember that $TOKENNAME must a Hex string.

```
TXIN=""
MINTERADDRESS=""
MINTERKEY="path/to/payment.skey"
POLICYID=""
TOKENNAME=""
MINT_SCRIPT_FILE="/path/to/ppbl-nft.json"
METADATA_JSON_FILE="/path/to/nft.json"
```

## Example Minting Transaction:
```
cardano-cli transaction build \
--babbage-era \
--testnet-magic 1097911063 \
--tx-in $TXIN \
--tx-out $MINTERADDRESS+"2000000 + 1 $POLICYID.$TOKENNAME" \
--mint "1 $POLICYID.$TOKENNAME" \
--mint-script-file $MINT_SCRIPT_FILE \
--change-address $MINTERADDRESS \
--metadata-json-file $METADATA_JSON_FILE \
--protocol-params-file protocol.json \
--out-file mint-ppbl-nft.raw

cardano-cli transaction sign \
--signing-key-file $MINTERKEY \
--testnet-magic 1097911063 \
--tx-body-file mint-ppbl-nft.raw \
--out-file mint-ppbl-nft.signed

cardano-cli transaction submit \
--tx-file mint-ppbl-nft.signed \
--testnet-magic 1097911063
```

## A great way to check your understanding: try to mint two NFTs in one transaction.
- In `nft.json`, note that addtional NFTs can created on the same policyId.
- [Lesson 201.3](https://gitlab.com/gimbalabs/plutus-pbl-summer-2022/ppbl-course-02/-/blob/master/project-02/docs/201-3.md) shows you how to mint multiple tokens at once.
- Try to mint several NFTs in one transaction - and bring your questions to Live Coding.

## Send your NFT to a browser wallet to view:

```
cardano-cli transaction build \
--babbage-era \
--testnet-magic 1097911063 \
--tx-in $TXIN1 \
--tx-in $TXIN2 \
--tx-out $ETERNL+"2000000 + 1 $POLICYID.$TOKENNAME" \
--change-address $MINTERADDRESS \
--out-file tx.draft

cardano-cli transaction sign \
--tx-body-file tx.draft \
--signing-key-file $MINTERKEY \
--testnet-magic 1097911063 \
--out-file tx.signed

cardano-cli transaction submit \
--testnet-magic 1097911063 \
--tx-file tx.signed
```