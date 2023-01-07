#!/usr/bin/env bash

# GBTE Transaction #2
# Contributor Commits to a Bounty

# Usage:
#. 02-contributor-commits-to-bounty (CONTRIBUTOR Address) (Path to CONTRIBUTOR Signing Key)

# Arguments
# export CONTRIBUTOR=$1
# export CONTRIBUTORKEY=$2
export CONTRIBUTOR=addr_test1vrtnnles3mxk8dy9fcrha8gl5la98hxwx00llc437kkjnhcqsxc8r
export CONTRIBUTORKEY=/opt/DEV/PLUTUS/tools/preprod-wallets/preprod1.skey

# Hard code these variables for your Bounty Treasury
# /opt/DEV/PLUTUS/ppbl-course-02/project-303/bounty-treasury-escrow/output/example-bounty-treasury-new-preprod.addr
TREASURY_ADDR=addr_test1wrk2n3ygme5jh05nm668eu26phljpg56pd8lts27j9ucc0qgc0ypz
# /opt/DEV/PLUTUS/ppbl-course-02/project-303/bounty-treasury-escrow/output/example-bounty-escrow-new-preprod.addr
BOUNTY_ADDR=addr_test1wzyvjgjxy5mr88ny3sm96qatd90fazsj625gxjr8hhrklqsf6ftxl
TREASURY_PLUTUS_SCRIPT="/opt/DEV/PLUTUS/ppbl-course-02/project-303/bounty-treasury-escrow/output/example-bounty-treasury-new-preprod.plutus"
BOUNTY_ASSET="fb45417ab92a155da3b31a8928c873eb9fd36c62184c736f189d334c.7447696d62616c"
TREASURY_DATUM="/opt/DEV/PLUTUS/ppbl-course-02/project-303/bounty-treasury-escrow/datum-and-redeemers/TreasuryDatumExample01.json"
TREASURY_ACTION="/opt/DEV/PLUTUS/ppbl-course-02/project-303/bounty-treasury-escrow/datum-and-redeemers/TreasuryActionExample01.json"
# BOUNTY_DATUM="/opt/DEV/PLUTUS/ppbl-course-02/project-303/bounty-treasury-escrow/datum-and-redeemers/BountyEscrowDatumExample01.json"
BOUNTY_DATUM="/opt/DEV/PLUTUS/ppbl-course-02/project-303/bounty-treasury-escrow/output/BountyEscrow-Redeemer-Sergi10.json"

# export CARDANO_NODE_SOCKET_PATH=<YOUR PATH TO>/testnet-pre-production/db/node.socket
# CLI query tip --testnet-magic 1
# CLI query protocol-parameters --testnet-magic 1 --out-file protocol.json

# CLI query utxo --testnet-magic 1 --address $TREASURY_ADDR
# echo "Specify the Treasury Contract UTXO:"
# read CONTRACT_TXIN
CONTRACT_TXIN=ef77fdee9129afcdc6b265f3e921a258e45618a9a804cfd131f6992ae50092c8#2
# echo "How many lovelace are currently in the Treasury?"
# read 
LOVELACE_AT_TREASURY=641000000
# echo "How many tgimbals are currently in the Treasury?"
# read BOUNTY_TOKENS_AT_TREASURY
BOUNTY_TOKENS_AT_TREASURY=12569


# CLI query utxo --testnet-magic 1 --address $CONTRIBUTOR

# echo "Specify a Collateral UTxO:"
# read COLLATERAL
COLLATERAL=128b5ae7bf206145283ceb24c00ba5b4f6b554edccd64c15e1428a39b58d9144#0

# echo "Specify a TXIN with Contributor Token:"
# read TXIN1
#PPBL2022AccessToken
#TXIN1=75a8ea9213734842113aa3ffcf7ace3dc3b8da9adf239f055c2102e9fe28a18f#0
#Sergi10GBTEpreprod
TXIN1=090ce52718e132fcd669cb8f8685e126b045b673e978f4bd7d6c2988e6c5647d#0

# echo "What is the Asset ID of your Contributor Token?"
# read CONTRIBUTOR_ASSET
#PPBL2022AccessToken (5050424c32303232416363657373546f6b656e)
#Sergi10GBTEpreprod (536572676931304742544570726570726f64)
CONTRIBUTOR_ASSET=738ec2c17e3319fa3e3721dbd99f0b31fce1b8006bb57fbd635e3784.536572676931304742544570726570726f64

# echo "Specify a TXIN with with additional lovelace (for tx fees):"
# read TXIN2
TXIN2=f9e763944de6ed04b4a0340034ed69154a0034c4afb2e76baa1053b541f3fee7#3

# echo "Amount of lovelace in this bounty:"
# read BOUNTY_LOVELACE
BOUNTY_LOVELACE=7500000

# echo "Number of tgimbals in this bounty:"
# read BOUNTY_AMOUNT
BOUNTY_AMOUNT=200

LOVELACE_BACK_TO_TREASURY=$(expr $LOVELACE_AT_TREASURY - $BOUNTY_LOVELACE)
TOKENS_BACK_TO_TREASURY=$(expr $BOUNTY_TOKENS_AT_TREASURY - $BOUNTY_AMOUNT)


echo --babbage-era \
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
