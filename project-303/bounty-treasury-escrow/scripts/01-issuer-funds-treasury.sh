#!/usr/bin/env bash

# GBTE Transaction #1
# Issuer Locks Funds in Treasury

# Usage:
#. 01-issuer-funds-treasury (Issuer Address) (Path to Issuer Signing Key)

# Arguments
export ISSUER=$1
export ISSUERKEY=$2

# Hard code these variables for your Bounty Treasury
TREASURY_ADDR=
BOUNTY_ASSET=
DATUM_FILE=""

export CARDANO_NODE_SOCKET_PATH=<YOUR PATH TO>/db/node.socket
cardano-cli query tip --testnet-magic 1
cardano-cli query protocol-parameters --testnet-magic 1 --out-file protocol.json

cardano-cli query utxo --testnet-magic 1 --address $ISSUER

echo "Specify a TXIN with tgimbals:"
read TXIN1
echo "Specify a TXIN with with additional lovelace:"
read TXIN2
echo "Number of bounty tokens to lock:"
read LOCK_AMOUNT
echo "Number of bounty tokens back to Issuer:"
read TOKENS_BACK_TO_ISSUER

echo "Thanks, using $TXIN1 and $TXIN2"

cardano-cli transaction build \
--alonzo-era \
--tx-in $TXIN1 \
--tx-in $TXIN2 \
--tx-out $TREASURY_ADDR+"100000000 + $LOCK_AMOUNT $BOUNTY_ASSET" \
--tx-out-datum-embed-file $DATUM_FILE \
--tx-out $ISSUER+"2000000 + $TOKENS_BACK_TO_ISSUER $BOUNTY_ASSET" \
--change-address $ISSUER \
--protocol-params-file protocol.json \
--out-file fund-treasury.draft \
--testnet-magic 1

cardano-cli transaction sign \
--signing-key-file $ISSUERKEY \
--testnet-magic 1 \
--tx-body-file fund-treasury.draft \
--out-file fund-treasury.signed

cardano-cli transaction submit \
--tx-file fund-treasury.signed \
--testnet-magic 1
