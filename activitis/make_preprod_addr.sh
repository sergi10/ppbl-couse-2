#!/bin/bash
#cd "${0%/*}"

# $DIR = $1
# $NAME = $2
mkdir $1
cd $1

# Generate payment key-pair
CLI  address key-gen \
--verification-key-file buyer.vkey \
--signing-key-file buyer.skey 

# Generate wallet address
CLI  address build \
--payment-verification-key-file buyer.vkey \
--out-file buyer.addr \
--testnet-magic 1