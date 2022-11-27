#!/bin/bash
cd "${0%/*}"

DIR = $1
NAME = $2
mkdir $DIR
cd $DIR

# Generate payment key-pair
CLI address key-gen \
--verification-key-file $NAME.vkey \
--signing-key-file $NAME.skey 

# Generate wallet address
CLI address build \
--payment-verification-key-file $NAME.vkey \
--out-file $NAME.addr \
--testnet-magic 1