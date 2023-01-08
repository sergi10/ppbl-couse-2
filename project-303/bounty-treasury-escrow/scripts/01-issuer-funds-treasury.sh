#!/usr/bin/env bash

# GBTE Transaction #1
# Issuer Locks Funds in Treasury

# Usage:
#. 01-issuer-funds-treasury (Issuer Address) (Path to Issuer Signing Key)

# Arguments
ISSUER=addr_test1vrtnnles3mxk8dy9fcrha8gl5la98hxwx00llc437kkjnhcqsxc8r
ISSUERKEY="/opt/DEV/PLUTUS/tools/preprod-wallets/preprod1.skey"

# Hard code these variables for your Bounty Treasury
TREASURY_ADDR=addr_test1wzmchhafv9mj6fjaeu9qqvsw3estnlfd9242hh6943gf2jqnla7g8
BOUNTY_ASSET="bda714dac42c0c1c8303cf1b109b18cdfd04f8a578432895ac8e1ee4.7453657267693130"
DATUM_FILE="/opt/DEV/PLUTUS/ppbl-course-02/project-303/bounty-treasury-escrow/output/Sergi10-TreasuryDatum.json"

# export CARDANO_NODE_SOCKET_PATH=<YOUR PATH TO>/testnet-pre-production/db/node.socket
CLI query tip --testnet-magic 1
CLI query protocol-parameters --testnet-magic 1 --out-file protocol.json

CLI query utxo --testnet-magic 1 --address $ISSUER

# echo "Specify a TXIN with tSergi10:"
# read TXIN1
TXIN1=c0149fce6c1cede73cb5097411ebe9d67e9cea7938aeeeab9a7c21f115035299#1
# echo "Specify a TXIN with with additional lovelace:"
# read TXIN2
TXIN2=d39539c845b41c96588a64b664b50fd718a0c04588e6dc7106871a064edc3de3#0
# echo "Number of lovelace to lock:"
# read LOVELACE_LOCK_AMOUNT
LOVELACE_LOCK_AMOUNT=25000000
# We do not need to calculate number lovelace back to Issuer because --change-address handles this.
# echo "Number of bounty tokens to lock:"
# read LOCK_AMOUNT
LOCK_AMOUNT=250
# echo "Number of bounty tokens back to Issuer:"
# read TOKENS_BACK_TO_ISSUER
TOKENS_BACK_TO_ISSUER=99750

echo "Thanks, using $TXIN1 and $TXIN2"

CLI transaction build \
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

CLI transaction sign \
--signing-key-file $ISSUERKEY \
--testnet-magic 1 \
--tx-body-file fund-treasury.draft \
--out-file fund-treasury.signed

CLI transaction submit \
--tx-file fund-treasury.signed \
--testnet-magic 1
