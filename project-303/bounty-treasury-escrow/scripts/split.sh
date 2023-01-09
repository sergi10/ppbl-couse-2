# Arguments
# CLI query utxo --testnet-magic 1 --address addr_test1vqlt502gqt8t3whc0ldhrzmgsp6grgdtnpz2x2f05hhew8g0ylua5
# CONTRIBUTOR=addr_test1vqlt502gqt8t3whc0ldhrzmgsp6grgdtnpz2x2f05hhew8g0ylua5
# CONTRIBUTORKEY="/opt/DEV/PLUTUS/tools/preprod-wallets/preprod2.skey"
# TXTOK=55161888534668341f835aa03643bebd2020bbd82b38f057a155fecb26453088#0 
# TXIN1=83563d7029d148c8bb7b6ce45cae0bab4f0dac8968e3021b8bbeb97cb6cb272f#2

# CLI transaction build \
# --babbage-era \
# --tx-in $TXIN1 \
# --tx-in $TXTOK \
# --tx-out $CONTRIBUTOR+"5000000 + 1 bda714dac42c0c1c8303cf1b109b18cdfd04f8a578432895ac8e1ee4.7453657267693130414343455353544f4b454e" \
# --tx-out $CONTRIBUTOR+"5000000 + 3 bda714dac42c0c1c8303cf1b109b18cdfd04f8a578432895ac8e1ee4.7453657267693130414343455353544f4b454e" \
# --change-address $CONTRIBUTOR \
# --protocol-params-file protocol.json \
# --testnet-magic 1 \
# --out-file split-tx.draft

CLI query utxo --testnet-magic 1 --address addr_test1vrtnnles3mxk8dy9fcrha8gl5la98hxwx00llc437kkjnhcqsxc8r
CONTRIBUTOR=addr_test1vrtnnles3mxk8dy9fcrha8gl5la98hxwx00llc437kkjnhcqsxc8r
CONTRIBUTORKEY="/opt/DEV/PLUTUS/tools/preprod-wallets/preprod1.skey"
TXTOK=55861f37fb30b232dabd504484daef08dbdc54697becbac05b165196d85a118d#1 
TXIN1=4b48b8a29e54a251dec72bcb01cdd7b675313018fe95f3673cf42b34c2205fd4#0

CLI transaction build \
--babbage-era \
--tx-in $TXIN1 \
--tx-in $TXTOK \
--tx-out $CONTRIBUTOR+"5000000 + 1 bda714dac42c0c1c8303cf1b109b18cdfd04f8a578432895ac8e1ee4.7453657267693130414343455353544f4b454e" \
--tx-out $CONTRIBUTOR+"5000000 + 1 bda714dac42c0c1c8303cf1b109b18cdfd04f8a578432895ac8e1ee4.7453657267693130414343455353544f4b454e" \
--tx-out $CONTRIBUTOR+"5000000 + 78 bda714dac42c0c1c8303cf1b109b18cdfd04f8a578432895ac8e1ee4.7453657267693130414343455353544f4b454e" \
--change-address $CONTRIBUTOR \
--protocol-params-file protocol.json \
--testnet-magic 1 \
--out-file split-tx.draft


CLI transaction sign \
--signing-key-file $CONTRIBUTORKEY \
--testnet-magic 1 \
--tx-body-file split-tx.draft \
--out-file split-tx.signed

CLI transaction submit \
--tx-file split-tx.signed \
--testnet-magic 1