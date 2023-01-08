#!/usr/bin/env bash

# A script for minting tokens. Requires having Vim installed for the xxd utility

# HOW TO USE <======================================================================= ***
# This script can be used to mint any of the tokens needed to spin up a GBTE instance
# The type of token that you mint will be controlled by the script you pass in
# For more info and sample Policy Scripts, consult minting-tokens.md

echo "Be sure you understand the contents of minting-tokens.md prior to use."
# read -p "Press enter to continue"

# export CARDANO_NODE_SOCKET_PATH=<YOUR PATH TO>/db/node.socket
# CLI query tip --testnet-magic 1
# CLI query protocol-parameters --testnet-magic 1 --out-file protocol.json 

# Parameters: Senders public address and path to signing key
SENDER=addr_test1vrtnnles3mxk8dy9fcrha8gl5la98hxwx00llc437kkjnhcqsxc8r
SENDERKEY="/opt/DEV/PLUTUS/tools/preprod-wallets/preprod1.skey"
# d739ff308ecd63b4854e077e9d1fa7fa53dcce33dfffe2b1f5ad29df

# Optional Parameter, path to a second signing key
# export SECONDSIGNER=$3

# If a second signer has been passed in, this creates the necessary transaction components
# if [ -n "$SECONDSIGNER" ]; then
#     export BUILDSIGNER="--required-signer $SECONDSIGNER"
#     export SIGNING="--signing-key-file $SENDERKEY --signing-key-file $SECONDSIGNER"
# else
#     export BUILDSIGNER=""
#     export SIGNING="--signing-key-file $SENDERKEY"
# fi
BUILDSIGNER=""
SIGNING="--signing-key-file $SENDERKEY"

# echo "Is there a time constraint on this Policy? (yes/no, default no)"
# read TIMECONSTRAINT

# IF a time constraint is specified then this will add the necessary build components
# if [ $TIMECONSTRAINT == "yes" ]; then
#     echo "1 = invalid-before, 2 = invalid-hereafter (default 1)"
#     read BEFOREAFTER
#     if [ $BEFOREAFTER == 2 ]; then
#         BUILDTIME="--invalid-hereafter "
#     else
#         BUILDTIME="--invalid-before "
#     fi
#     echo "Enter slot number"
#     read TIMESLOT
#     export BUILDTIME+="$TIMESLOT"
# else
#     export BUILDTIME=""
# fi
# TIMESLOT= 17433040
# BUILDTIME="--invalid-hereafter 17433040"
BUILDTIME="--invalid-before 17516718"
# This is the full path to your policy script
# echo "Specify the path to a Policy Script"
# read SCRIPTFILE
# SCRIPTFILE="/opt/DEV/PLUTUS/ppbl-course-02/activitis/porject-303.2/tSergi10.script"
SCRIPTFILE="/opt/DEV/PLUTUS/ppbl-course-02/activitis/porject-303.2/Sergi10AsIssuer.script"
# transaction policyid --script-file ./policy/policy.script > policy/policyID

# POLICY_ID=$(CLI transaction policyid --script-file $SCRIPTFILE > /opt/DEV/PLUTUS/ppbl-course-02/activitis/porject-303.2/tSergi10.policyID)
# CLI transaction policyid --script-file /opt/DEV/PLUTUS/ppbl-course-02/activitis/porject-303.2/Sergi10AsIssuer.script > /opt/DEV/PLUTUS/ppbl-course-02/activitis/porject-303.2/tSergi10AsIssuer.policyID
POLICY_ID=c09c50bf1f23530376a2bd14611c64eb0f0bd4f197a402a6688b5c70

# echo "Specify the name of the token(s) to be minted in plain text"
# read TOKENNAME
TOKENNAME="tSergi10AsIssuer"

# echo "How many of these tokens should be minted?"
# read QTY_TOKENS
QTY_TOKENS=100

# CLI query utxo --testnet-magic 1 --address $SENDER

# echo "Which TXIN?"
# read TXIN
TXIN=55861f37fb30b232dabd504484daef08dbdc54697becbac05b165196d85a118d#0

echo "Thanks, using $TXIN"
echo "Minting $TOKENNAME"

# convert TOKENAME to HEX
TOKENHEXSTRING=$(xxd -pu <<< $TOKENNAME)
TOKENHEX=${TOKENHEXSTRING::-2}

CLI transaction build \
--babbage-era \
--testnet-magic 1 \
--tx-in $TXIN $BUILDSIGNER \
--tx-out $SENDER+"10000000 + 50 $POLICY_ID.$TOKENHEX" \
--tx-out addr_test1qpyk0j9upw3khw88a8fda5zmgp7p5frz3tkz5qnmczeprd2n2ntn8fs5mgkw9zmsq8730pnga7se4uyqejw2756knzws0uwz8p+"10000000 + 25 $POLICY_ID.$TOKENHEX" \
--tx-out addr_test1qzkxt9tcagpw5myjtmae5wau4tkv5hu5lr5pagagc4h7x6mhy35hn2pwkwh3uyz32tg4507cl6wh9d3p2gcgryfyqfcsaumm53+"10000000 + 25 $POLICY_ID.$TOKENHEX" \
--mint "$QTY_TOKENS $POLICY_ID.$TOKENHEX" \
--mint-script-file $SCRIPTFILE \
--change-address $SENDER $BUILDTIME \
--protocol-params-file protocol.json \
--out-file mint-${TOKENNAME}.draft

CLI transaction sign \
$SIGNING --testnet-magic 1 \
--tx-body-file mint-${TOKENNAME}.draft \
--out-file mint-${TOKENNAME}.signed

CLI transaction submit \
--tx-file mint-${TOKENNAME}.signed \
--testnet-magic 1

# CLI query utxo --testnet-magic 1 --address $SENDER