#!/usr/bin/env bash

# GBTE Transaction #2
# Contributor Commits to a Bounty

# Usage:
#. 02-contributor-commits-to-bounty (CONTRIBUTOR Address) (Path to CONTRIBUTOR Signing Key)

# Arguments
export CONTRIBUTOR=$1
export CONTRIBUTORKEY=$2

# Hard code these variables for your Bounty Treasury
TREASURY_ADDR=
BOUNTY_ADDR=
TREASURY_PLUTUS_SCRIPT=""
BOUNTY_ASSET=
TREASURY_DATUM=""
BOUNTY_DATUM=""

export CARDANO_NODE_SOCKET_PATH=<YOUR PATH TO>/db/node.socket
cardano-cli query tip --testnet-magic 1
cardano-cli query protocol-parameters --testnet-magic 1 --out-file protocol.json

cardano-cli query utxo --testnet-magic 1 --address $TREASURY_ADDR
echo "Specify the Treasury Contract UTXO:"
read CONTRACT_TXIN
echo "How many lovelace are currently in the Treasury?"
read LOVELACE_AT_TREASURY
echo "How many tgimbals are currently in the Treasury?"
read BOUNTY_TOKENS_AT_TREASURY


cardano-cli query utxo --testnet-magic 1 --address $CONTRIBUTOR

echo "Specify a Collateral UTxO:"
read COLLATERAL
echo "Specify a TXIN with Contributor Token:"
read TXIN1
echo "What is the Asset ID of your Contributor Token?"
read CONTRIBUTOR_ASSET
echo "Specify a TXIN with with additional lovelace (for tx fees):"
read TXIN2
echo "Amount of lovelace in this bounty:"
read BOUNTY_LOVELACE
echo "Number of tgimbals in this bounty:"
read BOUNTY_AMOUNT

LOVELACE_BACK_TO_TREASURY=$(expr $LOVELACE_AT_TREASURY - $BOUNTY_LOVELACE)
TOKENS_BACK_TO_TREASURY=$(expr $BOUNTY_TOKENS_AT_TREASURY - $BOUNTY_AMOUNT)

cardano-cli transaction build \
--alonzo-era \
--tx-in $CONTRACT_TXIN \
--tx-in-script-file $TREASURY_PLUTUS_SCRIPT \
--tx-in-datum-file $TREASURY_DATUM \
--tx-in-redeemer-file $BOUNTY_DATUM \
--tx-in $TXIN1 \
--tx-in $TXIN2 \
--tx-in-collateral $COLLATERAL \
--tx-out $BOUNTY_ADDR+"$BOUNTY_LOVELACE + 1 $CONTRIBUTOR_ASSET + $BOUNTY_AMOUNT $BOUNTY_ASSET" \
--tx-out-datum-embed-file $BOUNTY_DATUM \
--tx-out $TREASURY_ADDR+"$LOVELACE_BACK_TO_TREASURY + $TOKENS_BACK_TO_TREASURY $BOUNTY_ASSET" \
--tx-out-datum-embed-file $TREASURY_DATUM \
--change-address $CONTRIBUTOR \
--protocol-params-file protocol.json \
--testnet-magic 1 \
--out-file commitment-tx.draft

cardano-cli transaction sign \
--signing-key-file $CONTRIBUTORKEY \
--testnet-magic 1 \
--tx-body-file commitment-tx.draft \
--out-file commitment-tx.signed

cardano-cli transaction submit \
--tx-file commitment-tx.signed \
--testnet-magic 1
