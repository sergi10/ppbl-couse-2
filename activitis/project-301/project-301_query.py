import json, os, subprocess
from pprint import pprint

# region  GLOBALS
CARDANO_CLI_PATH = '/opt/adanode/bin/cardano-cli'
MAGICT = ' --testnet-magic 1 '

DIR = '/opt/DEV/PLUTUS/ppbl-course-02/activitis/project-301'
KEYS_DIR = '/opt/DEV/PLUTUS/tools/preprod-wallets/project-301'
SCRIPT_DIR = DIR
# SENDER = 'addr_test1vrtnnles3mxk8dy9fcrha8gl5la98hxwx00llc437kkjnhcqsxc8r'  # 'preprod1.addr'
SENDER_ADDR = 'preprod1.addr' 
SENDER_SKEY = 'preprod1.skey'
SENDER_VKEY = 'preprod1.vkey'
# RECEIVER = 'addr_test1vqlj4gyuhs5y6s4y350r0hhsgevfw46wl2hyhnssvnmjxqg8c90rj' # preprod2.addr
# RECEIVER = 'addr_test1qzkxt9tcagpw5myjtmae5wau4tkv5hu5lr5pagagc4h7x6mhy35hn2pwkwh3uyz32tg4507cl6wh9d3p2gcgryfyqfcsaumm53' # nami testnet
# RECEIVER2 ='addr1qx06ajxagpv8cq8uhun8y45d3vgscjpxaukxus35wc38pk7gzdza8fumastcs6j55660dqdgvs3mdn03cyrp8h0td0csrvpslz' # GameChanger.addr
RECEIVER = 'addr_test1qq4c2emcjmd9hyvwkm6cs6z6w8a0htw8wmnfc2pmmgtx8m2n2ntn8fs5mgkw9zmsq8730pnga7se4uyqejw2756knzws8gaak7' # Eternal preprod
RECEIVERR_ADDR = 'preprod2.addr' 

FILE_RAW = 'sc-301_tx.raw'
FILE_SINGNED = 'sc-301_tx.signed'
TOKEN1_AMOUNT = 1000
TOKEN2_AMOUNT = 100
TOKEN3_AMOUNT = 10
TOKEN_SEND = 500
TOKEN1 = 'Land'
TOKEN2 = 'Build'
TOKEN3 = 'Quarter'

POLICYID = 'bda714dac42c0c1c8303cf1b109b18cdfd04f8a578432895ac8e1ee4'
POLICYPATH = '/opt/DEV/PLUTUS/tools/preprod-wallets/token-native-script'
POLICYSCRIPT = 'ses-policy.script'
# protocol.json In the same path
PROTOCOL_FILE = 'protocol.json'
TOK_SKEY = 'preprod1.skey'

# endregion


# PROCESS TO GET ADDRESSES
# Read wallet address value from payment.addr file
with open(os.path.join(KEYS_DIR, SENDER_ADDR), 'r') as file:
    SENDER = file.read()
# with open(os.path.join(DIR, RECEIVERR_ADDR), 'r') as file:
#     RECEIVERR = file.read()
# print(SENDER)
# print(RECEIVERR)

#region AUXILIAR FUNCTIONS
def lovelance2ada(lovelance):
    return round(float(lovelance / 1000000), 6)

def ada2lovelance(ada):
    return int(ada * 1000000)   

def toHex(s):
    result = s
    if type(s) == str:
        result = s.encode('utf-8').hex()    
    return result

def tx2dict(txs):
    if type(txs) == list:
        result = {}
        transactions = []
        i = 0
        for tx in txs:
            assets = []
            procesed = {}
            # update({"role": "Developer"})
            procesed.update({'id': i})
            procesed.update({'tx': tx[0].decode()})
            procesed.update({'ix': tx[1].decode()})
            procesed.update({'lovelace': tx[2].decode()})
            if tx[5].decode() == 'TxOutDatumHash':
                script ={}
                script.update({'type': tx[6].decode()})
                script.update({'datumHash': tx[7].decode().replace('"','')})
                procesed.update({'script': script})
            elif tx[5].decode() != 'TxOutDatumNone':
                for h in range(5, len(tx) -2, 3):
                    token = {}
                    token.update({'asset': tx[h+1].decode()})
                    info = tx[h+1].decode().split('.')
                    if len(info) > 1:
                        token.update({'policyID': info[0]})
                        token.update({'name': info[1]})
                        token.update({'textname': bytes.fromhex(info[1]).decode('utf-8')})
                        token.update({'quantity': int(tx[h].decode())})
                        assets.append(token)
                    else:
                        script ={}
                        script.update({'type': tx[h+1].decode()})
                        script.update({'datumHash': tx[h+2].decode().replace('"','')})
                        procesed.update({'script': script}) 
            if assets:
                procesed.update({'assets': assets})
            transactions.append(procesed)
            i +=1
        result.update({'transactions': transactions})
        return result

def getUtxos(addr):
    rawUtxoTable = subprocess.check_output([
        CARDANO_CLI_PATH,
        'query', 'utxo',
        '--testnet-magic', str(1),
        '--address', addr])
    utxoTableRows = rawUtxoTable.strip().splitlines()[2:]
    txlist = []
    # totalLovelaceRecv = 0
    for row in utxoTableRows:
        line = row.split()
        txlist.append(line)
        # totalLovelaceRecv +=  int(line[2])
    if txlist:
        return tx2dict(txlist)

#endregion

#region PROCESS TO QUERY
# We tell python to execute cardano-cli shell command to query the UTXO and read the output data
# subprocess.call('command', shell=True, executable='/bin/bash')
wllt = 'addr_test1vrtnnles3mxk8dy9fcrha8gl5la98hxwx00llc437kkjnhcqsxc8r'
script_address = 'addr_test1wqy0j8ym4q3wutn05vkzxg8zddvgfp04l8f8pxjzh7a72dg3qxfvy'


# wallet_Txs = getUtxos(script_address)
# pprint(wallet_Txs)

#endregion



""" TOKENS
{'assets': [
    {'asset': 'bda714dac42c0c1c8303cf1b109b18cdfd04f8a578432895ac8e1ee4.4275696c64',
                               'name': '4275696c64',
                               'policyID': 'bda714dac42c0c1c8303cf1b109b18cdfd04f8a578432895ac8e1ee4',
                               'quantity': 100,
                               'textname': 'Build'},
    {'asset': 'bda714dac42c0c1c8303cf1b109b18cdfd04f8a578432895ac8e1ee4.4c616e64',
                               'name': '4c616e64',
                               'policyID': 'bda714dac42c0c1c8303cf1b109b18cdfd04f8a578432895ac8e1ee4',
                               'quantity': 1000,
                               'textname': 'Land'},
    {'asset': 'bda714dac42c0c1c8303cf1b109b18cdfd04f8a578432895ac8e1ee4.51756172746572',
                               'name': '51756172746572',
                               'policyID': 'bda714dac42c0c1c8303cf1b109b18cdfd04f8a578432895ac8e1ee4',
                               'quantity': 10,
                               'textname': 'Quarter'}],
"""

""" COLATERAL 

def collateral_transaction2():
    result = 'CLI transaction build --babbage-era  --testnet-magic 1'
    result += ' --tx-in ' + TX['tx']  + '#' + TX['ix'] 
    result += ' --tx-out ' + SENDER + '+' + str(int(ada2lovelance(5000)))
    result += ' --tx-out ' + SENDER + '+' + str(int(TX['lovelace']) - ada2lovelance(5000) -fee)+ '+"' 
    for asset in TX['assets']:
        result += str(asset['quantity']) + ' ' + asset['asset'] + ' + '
    result = result[:-3]
    result += '"'
    result += ' --change-address ' + SENDER 
    result += ' --out-file ' + SCRIPT_DIR + '/'+ FILE_RAW
    return result
# res = collateral_transaction2()
# print(res)
def sing_colateral_transaction2():
    result =  'CLI transaction sign'
    result += ' --signing-key-file ' + KEYS_DIR + '/' + TOK_SKEY
    result += ' --testnet-magic 1'
    result += ' --tx-body-file ' + SCRIPT_DIR + '/' + FILE_RAW
    result += ' --out-file ' + SCRIPT_DIR + '/' + FILE_SINGNED
    return result
# res = sing_colateral_transaction2()
# print(res)
def submit_metadata_transaction2():
    result =  'CLI transaction submit'
    result += ' --tx-file ' + SCRIPT_DIR + '/' + FILE_SINGNED
    result += ' --testnet-magic 1'
    return result
# res = submit_metadata_transaction2()
# print(res)
"""


#region SCRIPT VALIDATOR
# cd ppbl-course-02/activitis/project-02/plutus-scripts/
POLICYSCRIPT = 'ppbl-faucet-preprod-LAND.plutus'
# REDEEMERFILE = 'redeemer.json'
wllt = 'addr_test1vrtnnles3mxk8dy9fcrha8gl5la98hxwx00llc437kkjnhcqsxc8r'
script_address = 'addr_test1wr6ydfrw00eextwar9qqpg2clukee4s65z3egs824d7ftsqdpdnw6'
# datum_hash = '3ceb17d6702c23286dab4352f367a9614588d53099c39f31cd618e2851a56731'
fee = 187633 # ---187633

wallet_Txs = getUtxos(wllt)
pprint(wallet_Txs)
TX_TOKEN = wallet_Txs['transactions'][3]
TX_AUTH = wallet_Txs['transactions'][2]
TX_COL = wallet_Txs['transactions'][1]
script_Txs = getUtxos(script_address)
pprint(script_Txs)
TX_SC = script_Txs['transactions'][1]

"""
#region send tokens to script
FILE_TX = 'Tx_send_tokens.raw'
FILE_TX_SIGNED = 'Tx_send_tokens.signed'
def collateral_transaction2():
    result = 'CLI transaction build --babbage-era  --testnet-magic 1'
    result += ' --tx-in ' + TX_TOKEN['tx']  + '#' + TX_TOKEN['ix'] 
    result += ' --tx-in ' + TX_FEE['tx']  + '#' + TX_FEE['ix']
    # result += ' --tx-in-datum-value ' + str(1001)
    result += ' --tx-out ' + SENDER + '+' + str(int(ada2lovelance(50))) 
    result +=        '+"'  + str(TX_TOKEN['assets'][0]['quantity']) + ' ' + TX_TOKEN['assets'][0]['asset'] + '"' 
    result += ' --tx-out ' + SENDER + '+' + str(int(ada2lovelance(50))) 
    result +=        '+"'  + str(TX_TOKEN['assets'][1]['quantity'] - 60) + ' ' + TX_TOKEN['assets'][1]['asset'] + '"' 
    result += ' --tx-out ' + SENDER + '+' + str(int(ada2lovelance(50))) 
    result +=        '+"'  + str(TX_TOKEN['assets'][2]['quantity']) + ' ' + TX_TOKEN['assets'][2]['asset'] + '"' 
    result += ' --tx-out ' + SENDER + '+' + str(int(TX_TOKEN['lovelace']) + int(TX_FEE['lovelace']) - ada2lovelance(153) - fee) 
    result += ' --tx-out ' + script_address + '+' + str(int(ada2lovelance(3)))  
    result +=        '+"' + str(60) + ' ' + TX_TOKEN['assets'][1]['asset'] + '"' 
    result += ' --tx-out-datum-hash ' + 'c9f1a86686aecf55fd2f90d275c5c7908a2978df1aec9f189fcb3a00ff529ac0'
    result += ' --change-address ' + SENDER 
    result += ' --out-file ' + SCRIPT_DIR + '/'+ FILE_TX
    return result
res = collateral_transaction2()
print(res)
def sing_colateral_transaction2():
    result =  'CLI transaction sign'
    result += ' --signing-key-file ' + KEYS_DIR + '/' + TOK_SKEY
    result += ' --testnet-magic 1'
    result += ' --tx-body-file ' + SCRIPT_DIR + '/' + FILE_TX
    result += ' --out-file ' + SCRIPT_DIR + '/' + FILE_TX_SIGNED
    return result
res = sing_colateral_transaction2()
print(res)
def submit_metadata_transaction2():
    result =  'CLI transaction submit'
    result += ' --tx-file ' + SCRIPT_DIR + '/' + FILE_TX_SIGNED
    result += ' --testnet-magic 1'
    return result
res = submit_metadata_transaction2()
print(res)
#endregion
"""

# LOCK Transaction
fee = 0

def validator_transaction():
    result =  'CLI transaction build'
    result += ' --babbage-era' 
    result += ' --testnet-magic 1' 
    result += ' --tx-in ' + TX['tx']  + '#' + TX['ix']
    result += ' --tx-out ' + script_address + '+' + str(int(TX['lovelace']) - fee)
    result += ' --tx-out-datum-hash ' + datum_hash 
    result += ' --change-address ' + SENDER 
    # result += ' --protocol-params-file protocol.json' 
    result += ' --out-file ' + SCRIPT_DIR + '/'+ FILE_RAW
    return result
# res = validator_transaction()
# print(res)
#endregion

#region SCRIPT  UNLOCK FUNDS
script_hash = '94bfd0e3065d4e5446a38e41e861e12f43838c8ea9b5ebd72ed5eb6186837ac3'
script_ix = '0'
collateral_hash = ''
colalteral_ix = ''
ScriptDataInBabbageEra = '3ceb17d6702c23286dab4352f367a9614588d53099c39f31cd618e2851a56731'
file_wildraw_raw = 'sc_wildraw_tx.raw'
eternal_addr ='addr_test1qqwepf45p7vlyc9musyd49e836xpzqgppcu8cqu2uu0650jn2ntn8fs5mgkw9zmsq8730pnga7se4uyqejw2756knzwse4qq4m'

fee = 0
# RECIVE FAUCET TOKENS Transction
def wildraw_script_transaction():
    result =  'CLI transaction build'
    result += ' --babbage-era' 
    result += ' --testnet-magic 1'
    # send auth token TX_TOKEN
    result += ' --tx-in ' + TX_AUTH['tx']  + '#' + TX_AUTH['ix']
    result += ' --tx-in ' + TX_TOKEN['tx']  + '#' + TX_TOKEN['ix']
    # SC transaction
    result += ' --tx-in ' + TX_SC['tx']  + '#' + TX_SC['ix'] 
    result += ' --tx-in-script-file ' + SCRIPT_DIR + '/' + 'ppbl-faucet-preprod-LAND.plutus'
    result += ' --tx-in-datum-value ' + str(1001)
    result += ' --tx-in-redeemer-file ' + SCRIPT_DIR + '/' + 'redeemer_1001.json'
    result += ' --tx-in-collateral ' + TX_COL['tx']  + '#' + TX_COL['ix']
    result += ' --tx-out ' + SENDER + '+' + str(int(TX_TOKEN['lovelace']) - fee) 
    result +=        '+"'  + str(TX_TOKEN['assets'][0]['quantity'] + 10) + ' ' + TX_TOKEN['assets'][0]['asset'] + '"' 
    # result += ' --tx-out ' + SENDER + '+' + TX_COL['lovelace']
    result += ' --tx-out ' + SENDER + '+' + TX_AUTH['lovelace']
    result +=       '+"'  + str(TX_AUTH['assets'][0]['quantity'] + 1) + ' ' + TX_AUTH['assets'][0]['asset'] + '"'
    result += ' --tx-out ' + script_address + '+' + TX_SC['lovelace'] # script_address
    result +=        '+"'  + str(TX_SC['assets'][0]['quantity'] - 10) + ' ' + TX_SC['assets'][0]['asset'] + '"' 
    result += ' --tx-out-datum-hash ' + 'c9f1a86686aecf55fd2f90d275c5c7908a2978df1aec9f189fcb3a00ff529ac0'
    result += ' --change-address ' + SENDER 
    result += ' --protocol-params-file ' + KEYS_DIR + '/'+ 'protocol.json' 
    result += ' --out-file ' + SCRIPT_DIR + '/' + file_wildraw_raw
    return result
res = wildraw_script_transaction()
print(res)
#endregion



# res = make_transaction()
# print(res)
# res = sing_token_transaction()
# print(res)
# res = submit_token_transaction()
# print(res)

"""  OLD FUNCTIONS

cardano-cli query utxo --address $address --$testnet

CLI transaction build-raw
    --fee $fee
    --tx-in $txhash#$txix
    --tx-out $receiver+$receiver_output+"50 $policyid.$token1 + 33 $policyid.$token2"
    --tx-out $(cat tok.addr)+$output+"950 $policyid.$token1 + 967 $policyid.$token2"
    --out-file toktx.raw

fee=$(CLI transaction calculate-min-fee
  --tx-body-file toktx.raw
  --tx-in-count 1
  --tx-out-count 2
  --witness-count 1 $MAGICT
  --protocol-params-file protocol.json | cut -d " " -f1)

CLI transaction sign --signing-key-file tok.skey --signing-key-file policy/policy.skey $MAGICT --tx-body-file toktx.raw --out-file toktx.signed


CLI transaction sign --signing-key-file tok.skey --signing-key-file policy/policy.skey $MAGICT --tx-body-file toktx.raw --out-file toktx.signed

def make_transaction2():
    result = 'CLI transaction build --babbage-era  --testnet-magic 1'
    # result += ' --change-address ' + SENDER 
    # result += ' --fee ' + str(FEE)
    result += ' --tx-in ' + TXTOKEN + '#' + TXTOKENIX
    result += ' --tx-out ' + SENDER + '+' + str(MIN_SEND) + '+"' + str(
        1) + ' ' + POLICYID + '.' + TOKEN_S1 + '"'
    result += ' --tx-out ' + RECEIVER + '+' + str(SENDER_AMOUNT - MIN_SEND - FEE) + '+"' + str(
        1) + ' ' + POLICYID + '.' + TOKEN_S1 + '"'
    result += ' --out-file ' + FILE_RAW
    return result

def sing():
    result = 'CLI transaction sign'
    result += ' --signing-key-file ' + SENDER_SKEY
    # result += ' --signing-key-file ' + POL_SKEY
    result += MAGICT
    result += ' --tx-body-file ' + FILE_RAW
    result += ' --out-file ' + FILE_SINGNED
    return result


def submit():
    # CLI     transaction    submit - -tx - file    toktx.signed  $MAGICT
    result = 'CLI transaction submit'
    result += ' --tx-file ' + FILE_SINGNED + MAGICT
    return result


def burnign():
    result = 'CLI transaction build-raw'
    result += ' --fee ' + str(FEE)
    result += ' --tx-in ' + TXHASH_TOK + '#' + TXIX
    result += ' --tx-out ' + RECEIVER + '+' + str(
        RECEIVER_AMOUNT - FEE) + '+"40 ' + POLICYID + '.' + TOKEN1 + ' + ' + '319 ' \
              + POLICYID + '.' + TOKEN2 + '"'
    result += ' --mint=' + '"-1 ' + POLICYID + '.' + TOKEN1 + \
              ' + ' + '-1 ' + POLICYID + '.' + TOKEN2 + '"'
    result += ' --minting-script-file policy/policy.script'
    # result += ' --metadata-json-file tokens.json'
    result += ' --out-file ' + FILE_RAW
    return result


def mint():
    assets = ['AAA', 'BBB', 'CCC', 'DDD', 'EEE', 'FFF', 'GGG', 'HHH']
    amounts = [1, 20, 400, 4000, 40000, 200000, 1000000, 5000000]
    mint = ''
    for a in assets:
        mint += str(amounts[assets.index(a)]) + ' ' + POLICYID + '.' + a + ' '

    result = 'CLI transaction build-raw'
    result += ' --fee ' + str(FEE)
    result += ' --tx-in ' + TXHASH_TOK + '#' + TXIX
    result += ' --tx-out ' + RECEIVER + '+' + str(MIN_SEND) + '+"' + str(
        TOKEN1_AMOUNT) + ' ' + POLICYID + '.' + TOKEN1 + ' + ' + str(TOKEN2_AMOUNT) \
              + ' ' + POLICYID + '.' + TOKEN2 + '"'
    result += ' --mint="' + mint
    result += ' --minting-script-file ' + FILE_SCRIPT
    result += ' --out-file ' + FILE_RAW
    # result += ' --metadata-json-file ' + 'mint.json'
    return result


def createPolicyID():
    # \"$(cardano-cli address key-hash --payment-verification-key-file policy/policy.vkey)\"
    hash_policyID = '920ffdb246cb66dfdd74e29f76529f75a0a4aaaeebcce12a9c7f44cb'
    # \"slot\": $(expr $(cardano-cli query tip --mainnet | jq .slot?) + 10000)"
    # CLI query tip $MAGICT | jq.slot --> 40008254
    slot = 1000

    result = {"type": "all",
              "scripts": [
                  {
                      "type": "before",
                      "slot": slot
                  },
                  {
                      "type": "sig",
                      "keyHash": hash_policyID
                  }
              ]
              }

    return result

print(make_transaction())
print(make_draft_transaction())
print(calc_fee())
print(sing())
print(submit())
print(burnign())
pprint(mint())
pprint(json.dumps(createPolicyID()))

"""
