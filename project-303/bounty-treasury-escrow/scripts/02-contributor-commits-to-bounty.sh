#!/usr/bin/env bash

# GBTE Transaction #2
# Contributor Commits to a Bounty

# Usage:
#. 02-contributor-commits-to-bounty (CONTRIBUTOR Address) (Path to CONTRIBUTOR Signing Key)

# Arguments
CONTRIBUTOR=addr_test1vqlt502gqt8t3whc0ldhrzmgsp6grgdtnpz2x2f05hhew8g0ylua5
CONTRIBUTORKEY="/opt/DEV/PLUTUS/tools/preprod-wallets/preprod2.skey"

# Hard code these variables for your Bounty Treasury
TREASURY_ADDR=addr_test1wzmchhafv9mj6fjaeu9qqvsw3estnlfd9242hh6943gf2jqnla7g8
BOUNTY_ADDR=addr_test1wz9vqh0paq8wyp4ywfw409fe3eep28uvu3ae7v48l2lr83c0thhcu
TREASURY_PLUTUS_SCRIPT="/opt/DEV/PLUTUS/ppbl-course-02/project-303/bounty-treasury-escrow/output/sergi10-bounty-treasury-preprod.plutus"
BOUNTY_ASSET="bda714dac42c0c1c8303cf1b109b18cdfd04f8a578432895ac8e1ee4.7453657267693130"
TREASURY_DATUM="/opt/DEV/PLUTUS/ppbl-course-02/project-303/bounty-treasury-escrow/output/Sergi10-TreasuryDatum.json"
TREASURY_ACTION="/opt/DEV/PLUTUS/ppbl-course-02/project-303/bounty-treasury-escrow/output/Sergi10-TreasuryAction.json"
BOUNTY_DATUM="/opt/DEV/PLUTUS/ppbl-course-02/project-303/bounty-treasury-escrow/output/Sergi10-BountyEscrow-Datum.json"

# export CARDANO_NODE_SOCKET_PATH=<YOUR PATH TO>/testnet-pre-production/db/node.socket
# CLI query tip --testnet-magic 1
# CLI query protocol-parameters --testnet-magic 1 --out-file protocol.json

# CLI query utxo --testnet-magic 1 --address $TREASURY_ADDR
# echo "Specify the Treasury Contract UTXO:"
# read CONTRACT_TXIN
CONTRACT_TXIN=c4c9cb417568ab8ba29a20406566a32ff9c06bd258c397a1cca6e16fa7388c84#1
# echo "How many lovelace are currently in the Treasury?"
# read LOVELACE_AT_TREASURY
LOVELACE_AT_TREASURY=25000000
# echo "How many tSergi10 are currently in the Treasury?"
# read BOUNTY_TOKENS_AT_TREASURY
BOUNTY_TOKENS_AT_TREASURY=250


CLI query utxo --testnet-magic 1 --address $CONTRIBUTOR

# echo "Specify a Collateral UTxO:"
# read COLLATERAL
COLLATERAL=c123312a1f7d80ef86f0c3392ec46d6bff768099d826cdf8ed4fe81b2930ba90#0
# echo "Specify a TXIN with Contributor Token:"
# read TXIN1
TXIN1=83563d7029d148c8bb7b6ce45cae0bab4f0dac8968e3021b8bbeb97cb6cb272f#1
# echo "What is the Asset ID of your Contributor Token?"
# read CONTRIBUTOR_ASSET
CONTRIBUTOR_ASSET=bda714dac42c0c1c8303cf1b109b18cdfd04f8a578432895ac8e1ee4.7453657267693130414343455353544f4b454e
# echo "Specify a TXIN with with additional lovelace (for tx fees):"
# read TXIN2
TXIN2=83563d7029d148c8bb7b6ce45cae0bab4f0dac8968e3021b8bbeb97cb6cb272f#0
# echo "Amount of lovelace in this bounty:"
# read BOUNTY_LOVELACE
BOUNTY_LOVELACE=10000000
# echo "Number of tSergi10 in this bounty:"
# read BOUNTY_AMOUNT
BOUNTY_AMOUNT=100

LOVELACE_BACK_TO_TREASURY=$(expr $LOVELACE_AT_TREASURY - $BOUNTY_LOVELACE)
TOKENS_BACK_TO_TREASURY=$(expr $BOUNTY_TOKENS_AT_TREASURY - $BOUNTY_AMOUNT)

CLI transaction build \
--babbage-era \
--tx-in $CONTRACT_TXIN \
--tx-in-script-file $TREASURY_PLUTUS_SCRIPT \
--tx-in-datum-file $TREASURY_DATUM \
--tx-in-redeemer-file $TREASURY_ACTION \
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

CLI transaction sign \
--signing-key-file $CONTRIBUTORKEY \
--testnet-magic 1 \
--tx-body-file commitment-tx.draft \
--out-file commitment-tx.signed

CLI transaction submit \
--tx-file commitment-tx.signed \
--testnet-magic 1
