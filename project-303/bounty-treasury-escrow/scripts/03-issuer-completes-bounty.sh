#!/usr/bin/env bash

# GBTE Transaction #3
# Issuer Unlocks a Bounty

# Usage:
#. 03-contributor-commits-to-bounty (ISSUER Address) (Path to ISSUER Signing Key) (CONTRIBUTOR Address)

# Arguments
ISSUER=addr_test1vrtnnles3mxk8dy9fcrha8gl5la98hxwx00llc437kkjnhcqsxc8r
ISSUERKEY="/opt/DEV/PLUTUS/tools/preprod-wallets/preprod1.skey"
# CONTRIBUTOR=addr_test1vqlt502gqt8t3whc0ldhrzmgsp6grgdtnpz2x2f05hhew8g0ylua5
CONTRIBUTOR=addr_test1vrtnnles3mxk8dy9fcrha8gl5la98hxwx00llc437kkjnhcqsxc8r

BOUNTY_ADDR=addr_test1wz9vqh0paq8wyp4ywfw409fe3eep28uvu3ae7v48l2lr83c0thhcu
BOUNTY_PLUTUS_SCRIPT="/opt/DEV/PLUTUS/ppbl-course-02/project-303/bounty-treasury-escrow/output/sergi10-bounty-escrow-preprod.plutus"
BOUNTY_ASSET=bda714dac42c0c1c8303cf1b109b18cdfd04f8a578432895ac8e1ee4.7453657267693130
BOUNTY_DATUM="/opt/DEV/PLUTUS/ppbl-course-02/project-303/bounty-treasury-escrow/output/Sergi10-BountyEscrow-Datum.json"
# BOUNTY_DATUM="/opt/DEV/PLUTUS/ppbl-course-02/project-303/bounty-treasury-escrow/output/Sergi10-BountyEscrow-Contriubtor-Datum.json"
ACTION_JSON_FILE="/opt/DEV/PLUTUS/ppbl-course-02/project-303/bounty-treasury-escrow/output/Distribute.json"

# export CARDANO_NODE_SOCKET_PATH=<YOUR PATH TO>/db/node.socket

# CLI query tip --testnet-magic 1
# CLI query protocol-parameters --testnet-magic 1 --out-file protocol.json

# This selection should match what is specified in your ACTION_JSON_FILE
# echo "What escrow action will you perform? (1=Cancel 2=Update 3=Distribute, default 3)"
# read ESCROW_ACTION
ESCROW_ACTION=3

# CLI query utxo --testnet-magic 1 --address $BOUNTY_ADDR
# echo "Which bounty utxo will you consume?"
# read CONTRACT_TXIN
CONTRACT_TXIN=55161888534668341f835aa03643bebd2020bbd82b38f057a155fecb26453088#1
# echo "How many lovelace are in this bounty?"
# read LOVELACE_IN_BOUNTY
LOVELACE_IN_BOUNTY=10000000
# echo "How many tSergi10 are in this bounty?"
# read BOUNTY_TOKENS_IN_BOUNTY
BOUNTY_TOKENS_IN_BOUNTY=100
# echo "What is the Asset ID of the Contributor Token in this bounty?"
# read CONTRIBUTOR_ASSET
CONTRIBUTOR_ASSET=bda714dac42c0c1c8303cf1b109b18cdfd04f8a578432895ac8e1ee4.7453657267693130414343455353544f4b454e

# CLI query utxo --testnet-magic 1 --address $ISSUER
# echo "Specify a Collateral UTxO:"
# read COLLATERAL
COLLATERAL=128b5ae7bf206145283ceb24c00ba5b4f6b554edccd64c15e1428a39b58d9144#0
# echo "Specify UTxO with Issuer Token:"
# read ISSUER_TOKEN_UTXO
ISSUER_TOKEN_UTXO=4b48b8a29e54a251dec72bcb01cdd7b675313018fe95f3673cf42b34c2205fd4#1
# echo "What is the Asset ID of Issuer Token?"
# read ISSUER_ASSET
ISSUER_ASSET=c09c50bf1f23530376a2bd14611c64eb0f0bd4f197a402a6688b5c70.74536572676931304173497373756572
# echo "Specify a TXIN for fees:"
# read TXIN1
TXIN1=fba8ec20c0a660bcaf3c424f8ea2c272ed5323891c8b0d40b964a6c37ef0cf41#0

#This block builds a transaction based on your selected action
if [ $ESCROW_ACTION == 1 ]; then
    echo "ESCROW_ACTION == 1"
    echo "Will the contributor token be returned (yes/no, default no)"
    read RETURN_CONTRIB
    if [ $RETURN_CONTRIB == "yes" ]; then
        export REMAINING_LOVELACE=$(expr $LOVELACE_IN_BOUNTY - 2000000)
        # export OUTPUTS="--tx-out $ISSUER+$REMAINING_LOVELACE + $BOUNTY_TOKENS_IN_BOUNTY $BOUNTY_ASSET --tx-out $CONTRIBUTOR+2000000 + 1 $CONTRIBUTOR_ASSET"
        CLI transaction build \
        --babbage-era \
        --tx-in $CONTRACT_TXIN \
        --tx-in-script-file $BOUNTY_PLUTUS_SCRIPT \
        --tx-in-datum-file $BOUNTY_DATUM \
        --tx-in-redeemer-file $ACTION_JSON_FILE \
        --tx-in $TXIN1 \
        --tx-in $ISSUER_TOKEN_UTXO \
        --tx-in-collateral $COLLATERAL \
        --tx-out $ISSUER+"$REMAINING_LOVELACE + $BOUNTY_TOKENS_IN_BOUNTY $BOUNTY_ASSET" --tx-out $CONTRIBUTOR+"2000000 + 1 $CONTRIBUTOR_ASSET" \
        --tx-out $ISSUER+"1500000 + 1 $ISSUER_ASSET" \
        --change-address $ISSUER \
        --protocol-params-file protocol.json \
        --testnet-magic 1 \
        --out-file distribute-bounty-tx.draft
    else
        # export OUTPUTS="--tx-out $ISSUER+\"$LOVELACE_IN_BOUNTY + $BOUNTY_TOKENS_IN_BOUNTY $BOUNTY_ASSET + 1 $CONTRIBUTOR_ASSET\""
        CLI transaction build \
        --babbage-era \
        --tx-in $CONTRACT_TXIN \
        --tx-in-script-file $BOUNTY_PLUTUS_SCRIPT \
        --tx-in-datum-file $BOUNTY_DATUM \
        --tx-in-redeemer-file $ACTION_JSON_FILE \
        --tx-in $TXIN1 \
        --tx-in $ISSUER_TOKEN_UTXO \
        --tx-in-collateral $COLLATERAL \
        --tx-out $ISSUER+"$LOVELACE_IN_BOUNTY + $BOUNTY_TOKENS_IN_BOUNTY $BOUNTY_ASSET + 1 $CONTRIBUTOR_ASSET" \
        --tx-out $ISSUER+"1500000 + 1 $ISSUER_ASSET" \
        --change-address $ISSUER \
        --protocol-params-file protocol.json \
        --testnet-magic 1 \
        --out-file distribute-bounty-tx.draft
    fi
elif [ ESCROW_ACTION == 2 ]; then
    echo "ESCROW_ACTION == 2"
    echo "Specify a TXIN with additional bounty tokens and/or ada:"
    read TXIN2
    echo "Enter new lovelace value in bounty"
    read NEW_LOVELACE
    echo "Enter new bounty token value in bounty"
    read NEW_TOKENS
    # export OUTPUTS="--tx-out $BOUNTY_ADDR+\"$NEW_LOVELACE + $NEW_TOKENS $BOUNTY_ASSET + 1 $CONTRIBUTOR_ASSET\" --tx-out-datum-embed-file $BOUNTY_DATUM"
    CLI transaction build \
    --babbage-era \
    --tx-in $CONTRACT_TXIN \
    --tx-in-script-file $BOUNTY_PLUTUS_SCRIPT \
    --tx-in-datum-file $BOUNTY_DATUM \
    --tx-in-redeemer-file $ACTION_JSON_FILE \
    --tx-in $TXIN1 \
    --tx-in $ISSUER_TOKEN_UTXO \
    --tx-in-collateral $COLLATERAL \
    --tx-out $BOUNTY_ADDR+"$NEW_LOVELACE + $NEW_TOKENS $BOUNTY_ASSET + 1 $CONTRIBUTOR_ASSET" --tx-out-datum-embed-file $BOUNTY_DATUM \
    --tx-out $ISSUER+"1500000 + 1 $ISSUER_ASSET" \
    --change-address $ISSUER \
    --protocol-params-file protocol.json \
    --testnet-magic 1 \
    --out-file distribute-bounty-tx.draft
else
    echo "ESCROW_ACTION == 3"
    # export OUTPUTS="--tx-out $CONTRIBUTOR+\"$LOVELACE_IN_BOUNTY + $BOUNTY_TOKENS_IN_BOUNTY $BOUNTY_ASSET + 1 $CONTRIBUTOR_ASSET\""
    
    CLI transaction build \
    --babbage-era \
    --tx-in $CONTRACT_TXIN \
    --tx-in-script-file $BOUNTY_PLUTUS_SCRIPT \
    --tx-in-datum-file $BOUNTY_DATUM \
    --tx-in-redeemer-file $ACTION_JSON_FILE \
    --tx-in $TXIN1 \
    --tx-in $ISSUER_TOKEN_UTXO \
    --tx-in-collateral $COLLATERAL \
    --tx-out $CONTRIBUTOR+"$LOVELACE_IN_BOUNTY + $BOUNTY_TOKENS_IN_BOUNTY $BOUNTY_ASSET + 1 $CONTRIBUTOR_ASSET" \
    --tx-out $ISSUER+"1500000 + 1 $ISSUER_ASSET" \
    --change-address $ISSUER \
    --protocol-params-file protocol.json \
    --testnet-magic 1 \
    --out-file distribute-bounty-tx.draft
    
fi


CLI transaction sign \
--signing-key-file $ISSUERKEY \
--testnet-magic 1 \
--tx-body-file distribute-bounty-tx.draft \
--out-file distribute-bounty-tx.signed

CLI transaction submit \
--tx-file distribute-bounty-tx.signed \
--testnet-magic 1
-embed-file $BOUNTY_DATUM
