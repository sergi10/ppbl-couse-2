#!/usr/bin/env bash

# GBTE Transaction #1
# Issuer Locks Funds in Treasury

# Usage:
#. 01-issuer-funds-treasury (Issuer Address) (Path to Issuer Signing Key)

# Arguments
export ISSUER=$1
export ISSUERKEY=$2

# Hard code these variables for your Bounty Treasury
TREASURY_ADDR=addr_test1wrk2n3ygme5jh05nm668eu26phljpg56pd8lts27j9ucc0qgc0ypz
BOUNTY_ASSET="fb45417ab92a155da3b31a8928c873eb9fd36c62184c736f189d334c.7447696d62616c"
DATUM_FILE="<YOUR PATH TO>/ppbl-course-02/project-303/bounty-treasury-escrow/output/jd-examples/TreasuryDatumExample01.json"

export CARDANO_NODE_SOCKET_PATH=<YOUR PATH TO>/testnet-pre-production/db/node.socket
cardano-cli query tip --testnet-magic 1
cardano-cli query protocol-parameters --testnet-magic 1 --out-file protocol.json

cardano-cli query utxo --testnet-magic 1 --address $ISSUER

echo "Specify a TXIN with tGimbals:"
read TXIN1
echo "Specify a TXIN with with additional lovelace:"
read TXIN2
echo "Number of lovelace to lock:"
read LOVELACE_LOCK_AMOUNT
# We do not need to calculate number lovelace back to Issuer because --change-address handles this.
echo "Number of bounty tokens to lock:"
read LOCK_AMOUNT
echo "Number of bounty tokens back to Issuer:"
read TOKENS_BACK_TO_ISSUER

echo "Thanks, using $TXIN1 and $TXIN2"

cardano-cli transaction build \
--babbage-era \
--tx-in $TXIN1 \
--tx-in $TXIN2 \
--tx-out $TREASURY_ADDR+"$LOVELACE_LOCK_AMOUNT + $LOCK_AMOUNT $BOUNTY_ASSET" \
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
