# Arguments
CONTRIBUTOR=addr_test1vqlt502gqt8t3whc0ldhrzmgsp6grgdtnpz2x2f05hhew8g0ylua5
CONTRIBUTORKEY="/opt/DEV/PLUTUS/tools/preprod-wallets/preprod2.skey"
TXIN1=b9098767b9e0579555bd84dc981fa8b86287205cc2bb6621037facdbfc85ba71#1

CLI transaction build \
--babbage-era \
--tx-in $TXIN1 \
--tx-in b9098767b9e0579555bd84dc981fa8b86287205cc2bb6621037facdbfc85ba71#0 \
--tx-out $CONTRIBUTOR+"2500000 + 1 bda714dac42c0c1c8303cf1b109b18cdfd04f8a578432895ac8e1ee4.7453657267693130414343455353544f4b454e" \
--tx-out $CONTRIBUTOR+"2500000 + 4 bda714dac42c0c1c8303cf1b109b18cdfd04f8a578432895ac8e1ee4.7453657267693130414343455353544f4b454e" \
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