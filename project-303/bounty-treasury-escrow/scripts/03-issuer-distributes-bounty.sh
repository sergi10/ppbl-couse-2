#!/usr/bin/env bash

# GBTE Transaction #3
# Issuer Unlocks a Bounty

# Usage:
#. 03-contributor-commits-to-bounty (ISSUER Address) (Path to ISSUER Signing Key) (CONTRIBUTOR Address)

# Arguments
export ISSUER=$1
export ISSUERKEY=$2
export CONTRIBUTOR=$3

BOUNTY_ADDR=
BOUNTY_PLUTUS_SCRIPT=""
BOUNTY_ASSET=
BOUNTY_DATUM=""
ACTION_JSON_FILE="<PATH TO>/Distribute.json"
ISSUER_PKH=

export CARDANO_NODE_SOCKET_PATH=<YOUR PATH TO>/db/node.socket
cardano-cli query tip --testnet-magic 1
cardano-cli query protocol-parameters --testnet-magic 1 --out-file protocol.json


cardano-cli query utxo --testnet-magic 1 --address $BOUNTY_ADDR
echo "Which bounty utxo will you distribute?"
read CONTRACT_TXIN
echo "How many lovelace are in this bounty?"
read LOVELACE_IN_BOUNTY
echo "How many tgimbals are in this bounty?"
read BOUNTY_TOKENS_IN_BOUNTY
echo "What is the Asset ID of the Contributor Token in this bounty?"
read CONTRIBUTOR_ASSET

cardano-cli query utxo --testnet-magic 1 --address $ISSUER
echo "Specify a Collateral UTxO:"
read COLLATERAL
echo "Specify a TXIN for fees:"
read TXIN1

cardano-cli transaction build \
--alonzo-era \
--tx-in $CONTRACT_TXIN \
--tx-in-script-file $BOUNTY_PLUTUS_SCRIPT \
--tx-in-datum-file $BOUNTY_DATUM \
--tx-in-redeemer-file $ACTION_JSON_FILE \
--tx-in $TXIN1 \
--tx-in-collateral $COLLATERAL \
--tx-out $CONTRIBUTOR+"$LOVELACE_IN_BOUNTY + $BOUNTY_TOKENS_IN_BOUNTY $BOUNTY_ASSET + 1 $CONTRIBUTOR_ASSET" \
--change-address $ISSUER \
--required-signer-hash $ISSUER_PKH \
--protocol-params-file protocol.json \
--testnet-magic 1 \
--out-file distribute-bounty-tx.draft

cardano-cli transaction sign \
--signing-key-file $ISSUERKEY \
--testnet-magic 1 \
--tx-body-file distribute-bounty-tx.draft \
--out-file distribute-bounty-tx.signed

cardano-cli transaction submit \
--tx-file distribute-bounty-tx.signed \
--testnet-magic 1
