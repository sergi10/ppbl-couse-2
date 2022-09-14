#!/usr/bin/env bash

# A script for minting tokens.
# Todo: Add comments

export ACCESS_TOKEN_POLICY_ID=738ec2c17e3319fa3e3721dbd99f0b31fce1b8006bb57fbd635e3784
export TOKEN_SUFFIX=GBTEpreprod

export CARDANO_NODE_SOCKET_PATH=<YOUR PATH TO>/db/node.socket
cardano-cli query tip --testnet-magic 1
cardano-cli query protocol-parameters --testnet-magic 1 --out-file protocol.json

# Parameters
export SENDER=$1
export SENDERKEY=$2
export CONTRIBUTOR=$3
export TOKENNAME=$4$TOKEN_SUFFIX

cardano-cli query utxo --testnet-magic 1 --address $SENDER

echo "Which TXIN?"
read TXIN

echo "Thanks, using $TXIN"
echo "Minting $TOKENNAME"
echo "Sending to $CONTRIBUTOR"

# convert TOKENAME to HEX
TOKENHEXSTRING=$(xxd -pu <<< $TOKENNAME)
export TOKENHEX=${TOKENHEXSTRING::-2}

cardano-cli transaction build \
--alonzo-era \
--testnet-magic 1 \
--tx-in $TXIN \
--tx-out $CONTRIBUTOR+"10000000 + 1 $ACCESS_TOKEN_POLICY_ID.$TOKENHEX" \
--mint "1 $ACCESS_TOKEN_POLICY_ID.$TOKENHEX" \
--mint-script-file gbte-contrib.script \
--change-address $SENDER \
--required-signer gbte-contrib.skey \
--protocol-params-file protocol.json \
--out-file mint-${TOKENNAME}.draft

cardano-cli transaction sign \
--signing-key-file $SENDERKEY \
--signing-key-file gbte-contrib.skey \
--testnet-magic 1 \
--tx-body-file mint-${TOKENNAME}.draft \
--out-file mint-${TOKENNAME}.signed

cardano-cli transaction submit \
--tx-file mint-${TOKENNAME}.signed \
--testnet-magic 1
