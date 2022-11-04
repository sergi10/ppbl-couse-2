#!/usr/bin/env bash

# A script for minting tokens. Requires having Vim installed for the xxd utility

# HOW TO USE <======================================================================= ***
# This script can be used to mint any of the tokens needed to spin up a GBTE instance
# The type of token that you mint will be controlled by the script you pass in
# For more info and sample Policy Scripts, consult minting-tokens.md

echo "Be sure you understand the contents of minting-tokens.md prior to use."
read -p "Press enter to continue"

export CARDANO_NODE_SOCKET_PATH=<YOUR PATH TO>/db/node.socket
cardano-cli query tip --testnet-magic 1
cardano-cli query protocol-parameters --testnet-magic 1 --out-file protocol.json 

# Parameters: Senders public address and path to signing key
export SENDER=$1
export SENDERKEY=$2

# Optional Parameter, path to a second signing key
export SECONDSIGNER=$3

# If a second signer has been passed in, this creates the necessary transaction components
if [ -n "$SECONDSIGNER" ]; then
    export BUILDSIGNER="--required-signer $SECONDSIGNER"
    export SIGNING="--signing-key-file $SENDERKEY --signing-key-file $SECONDSIGNER"
else
    export BUILDSIGNER=""
    export SIGNING="--signing-key-file $SENDERKEY"
fi


echo "Is there a time constraint on this Policy? (yes/no, default no)"
read TIMECONSTRAINT

# IF a time constraint is specified then this will add the necessary build components
if [ $TIMECONSTRAINT == "yes" ]; then
    echo "1 = invalid-before, 2 = invalid-hereafter (default 1)"
    read BEFOREAFTER
    if [ $BEFOREAFTER == 2 ]; then
        BUILDTIME="--invalid-hereafter "
    else
        BUILDTIME="--invalid-before "
    fi
    echo "Enter slot number"
    read TIMESLOT
    export BUILDTIME+="$TIMESLOT"
else
    export BUILDTIME=""
fi

# This is the full path to your policy script
echo "Specify the path to a Policy Script"
read SCRIPTFILE

POLICY_ID=$(cardano-cli transaction policyid --script-file $SCRIPTFILE)

echo "Specify the name of the token(s) to be minted in plain text"
read TOKENNAME

echo "How many of these tokens should be minted?"
read QTY_TOKENS

cardano-cli query utxo --testnet-magic 1 --address $SENDER

echo "Which TXIN?"
read TXIN

echo "Thanks, using $TXIN"
echo "Minting $TOKENNAME"

# convert TOKENAME to HEX
TOKENHEXSTRING=$(xxd -pu <<< $TOKENNAME)
export TOKENHEX=${TOKENHEXSTRING::-2}

cardano-cli transaction build \
--babbage-era \
--testnet-magic 1 \
--tx-in $TXIN $BUILDSIGNER \
--tx-out $SENDER+"10000000 + $QTY_TOKENS $POLICY_ID.$TOKENHEX" \
--mint "$QTY_TOKENS $POLICY_ID.$TOKENHEX" \
--mint-script-file $SCRIPTFILE \
--change-address $SENDER $BUILDTIME \
--protocol-params-file protocol.json \
--out-file mint-${TOKENNAME}.draft

cardano-cli transaction sign \
$SIGNING --testnet-magic 1 \
--tx-body-file mint-${TOKENNAME}.draft \
--out-file mint-${TOKENNAME}.signed

cardano-cli transaction submit \
--tx-file mint-${TOKENNAME}.signed \
--testnet-magic 1