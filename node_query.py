# cardano-cli query utxo --address $address --$testnet
#
# CLI transaction build-raw
#     --fee $fee
#     --tx-in $txhash#$txix
#     --tx-out $receiver+$receiver_output+"50 $policyid.$token1 + 33 $policyid.$token2"
#     --tx-out $(cat tok.addr)+$output+"950 $policyid.$token1 + 967 $policyid.$token2"
#     --out-file toktx.raw
#
# fee=$(CLI transaction calculate-min-fee
#   --tx-body-file toktx.raw
#   --tx-in-count 1
#   --tx-out-count 2
#   --witness-count 1 $MAGICT
#   --protocol-params-file protocol.json | cut -d " " -f1)
#
# CLI transaction sign --signing-key-file tok.skey --signing-key-file policy/policy.skey $MAGICT --tx-body-file toktx.raw --out-file toktx.signed
#
#
# CLI transaction sign --signing-key-file tok.skey --signing-key-file policy/policy.skey $MAGICT --tx-body-file toktx.raw --out-file toktx.signed

import json, os, subprocess
from pprint import pprint
import tabview as t



DIR = '/opt/DEV/PLUTUS/ppbl-course-02/'
DIR +='activitis/project-02/native-scripts'
TOKEN_FOLDER = '/token-native-script'
CARDANO_CLI_PATH = '/opt/adanode/bin/cardano-cli'
MAGICT = ' --testnet-magic 1 '

# SENDER = 'addr_test1vrtnnles3mxk8dy9fcrha8gl5la98hxwx00llc437kkjnhcqsxc8r'  # 'preprod1.addr'
SENDER_ADDR = 'preprod1.addr' 
SENDER_SKEY = 'preprod1.skey'
# RECEIVER = 'addr_test1vqlj4gyuhs5y6s4y350r0hhsgevfw46wl2hyhnssvnmjxqg8c90rj' # preprod2.addr
# RECEIVER = 'addr_test1qzkxt9tcagpw5myjtmae5wau4tkv5hu5lr5pagagc4h7x6mhy35hn2pwkwh3uyz32tg4507cl6wh9d3p2gcgryfyqfcsaumm53' # nami testnet
RECEIVER = 'addr_test1qq4c2emcjmd9hyvwkm6cs6z6w8a0htw8wmnfc2pmmgtx8m2n2ntn8fs5mgkw9zmsq8730pnga7se4uyqejw2756knzws8gaak7' # Eternal preprod
RECEIVERR_ADDR = 'preprod2.addr' 
# RECEIVER2 ='addr1qx06ajxagpv8cq8uhun8y45d3vgscjpxaukxus35wc38pk7gzdza8fumastcs6j55660dqdgvs3mdn03cyrp8h0td0csrvpslz' # GameChanger.addr
TXHASH = 'd1fdf7de73288ce9db78926ab7b716ecab63f31cddc13f35d8636670ac04a02d'
# TXHASH_TOK = '37008dfb97d76f1764a380b64968e3cf5734e0075ab42b7edc91c50ece8bea59'
# TXHASH_LOV = 'abf44cbf6abc68c929a33fbd6dbf1839256e7cd868b04c00b9c12c87bcc068e5'
# 'acfa9843ad0d11baa5f5e137e60f6c298569f1b59008e8b54d7c232c652da258'
# TXHASH
TXIX = '0'
FEE = 170869 # Lovelace 170869
SENDER_AMOUNT = 894113579
RECEIVER_AMOUNT = 2092503
MIN_SEND = 5000000

HASH = 'd739ff308ecd63b4854e077e9d1fa7fa53dcce33dfffe2b1f5ad29df'
FILE_RAW = 'tx.raw'
FILE_SINGNED = 'tx.signed'
FILE_MINT = 'mint.raw'



# PROCESS TO GET ADDRESSES
# Read wallet address value from payment.addr file
with open(os.path.join(DIR, SENDER_ADDR), 'r') as file:
    SENDER = file.read()
# with open(os.path.join(DIR, RECEIVERR_ADDR), 'r') as file:
#     RECEIVERR = file.read()
# print(SENDER)
# print(RECEIVERR)
# AUXILIAR FUNCTIONS
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
            procesed['id'] = i
            procesed['tx'] = tx[0].decode()
            procesed['ix'] = tx[1].decode()
            procesed['lovelace'] = tx[2].decode()
            if tx[5].decode() != 'TxOutDatumNone':
                for h in range(5, len(tx) -2, 3):
                    token = {}
                    token['asset'] = tx[h+1].decode()
                    info = tx[h+1].decode().split('.')
                    token['policyID'] = info[0]
                    token['name'] = info[1]
                    token['textname'] = bytes.fromhex(info[1]).decode('utf-8')
                    token['quantity'] = int(tx[h].decode())
                    assets.append(token)
            if assets:
                procesed['assets'] = assets
            transactions.append(procesed)
            i +=1
        result['transactions'] = transactions
        return result




# PROCESS TO QUERY
# We tell python to execute cardano-cli shell command to query the UTXO and read the output data
# subprocess.call('command', shell=True, executable='/bin/bash')
wllt = 'addr_test1vrtnnles3mxk8dy9fcrha8gl5la98hxwx00llc437kkjnhcqsxc8r'

rawUtxoTable = subprocess.check_output([
    CARDANO_CLI_PATH,
    'query', 'utxo',
    '--testnet-magic', str(1),
    '--address', wllt])
utxoTableRows = rawUtxoTable.strip().splitlines()[2:]
txlist = []
totalLovelaceRecv = 0
for row in utxoTableRows:
    line = row.split()
    # line = row.strip().split()
    # line = list(line)
    txlist.append(line)
    totalLovelaceRecv +=  int(line[2])
    # print(row) #, sep=' - ', end='')
    # for elem in row:
    #     print(elem) #, sep=' - ', end='')

# pprint(txlist)
# pprint(tx2dict(txlist))

wallet_Txs = tx2dict(txlist)

TX = wallet_Txs['transactions'][2]
TXHASH = TX['tx']
TXIX  = TX['ix']




TXHASH = txlist[1][0].decode()
TXIX  = txlist[1][1].decode()
TXHASH2 = txlist[2][0].decode()
TXIX2  = txlist[2][1].decode()

# t.view(txlist)


# print( str(TXHASH) +'#'+ str(TXIX))
# print(lovelance2ada(totalLovelaceRecv))
# print(ada2lovelance(2.152698764321))

#region MINTING NATIVE SCRIPT

#region VARIABLES
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
FILE_SCRIPT = 'policy/policy.script'
PROTOCOL_FILE = 'protocol.json'
TOK_SKEY = 'preprod1.skey'
# TOK_SKEY = 'test02.skey'
# TOK_SKEY = 'tok.skey'
POL_SKEY = 'policy/policy.skey'
#endregion
#region TRANSACTION
def mint_token_transaction():
    result =     'CLI transaction build'
    result +=    ' --babbage-era' 
    result +=    ' --testnet-magic 1' 
    # result +=    ' --fee ' + str(FEE) 
    result +=    ' --tx-in ' + TXHASH  + '#' + TXIX  
    result +=    ' --tx-out ' + SENDER + '+' + str(MIN_SEND) 
    result +=        '+"'  + str(TOKEN1_AMOUNT) + ' ' + POLICYID + '.' + toHex(TOKEN1) 
    result +=        ' + ' + str(TOKEN2_AMOUNT) + ' ' + POLICYID + '.' + toHex(TOKEN2) 
    result +=        ' + ' + str(TOKEN3_AMOUNT) + ' ' + POLICYID + '.' + toHex(TOKEN3) + '"' 
    result +=    ' --mint '+'"' + str(TOKEN1_AMOUNT) + ' ' + POLICYID + '.' + toHex(TOKEN1) 
    result +=        ' + ' + str(TOKEN2_AMOUNT) + ' ' + POLICYID + '.' + toHex(TOKEN2) 
    result +=        ' + ' + str(TOKEN3_AMOUNT) + ' ' + POLICYID + '.' + toHex(TOKEN3) + '"'
    result +=    ' --mint-script-file ' + POLICYSCRIPT 
    result +=    ' --change-address ' + SENDER 
    result +=    ' --protocol-params-file protocol.json' 
    result +=    ' --out-file ' + FILE_RAW
    return result
# Estimated transaction fee: Lovelace 172321
# res = mint_token_transaction()
# print(res)

def sing_token_transaction():
    result =  'CLI transaction sign'
    result += ' --signing-key-file ' + TOK_SKEY
    result += ' --testnet-magic 1'
    result += ' --tx-body-file ' + FILE_RAW
    result += ' --out-file ' + FILE_SINGNED
    return result


def submit_token_transaction():
    result =  'CLI transaction submit'
    result += ' --tx-file ' + FILE_SINGNED
    result += ' --testnet-magic 1'
    return result

#endregion


#region MINTING plutus SCRIPT 
#region COLATERAL

def collateral_transaction():
    result = 'CLI transaction build --babbage-era  --testnet-magic 1'
    result += ' --tx-in ' + TXHASH  + '#' + TXIX 
    result += ' --tx-out ' + SENDER + '+' + str(MIN_SEND)
    result += ' --change-address ' + SENDER 
    result += ' --out-file ' + FILE_RAW
    return result
# res = collateral_transaction()
# print(res)

def sing_colateral_transaction():
    result =  'CLI transaction sign'
    result += ' --signing-key-file ' + TOK_SKEY
    result += ' --testnet-magic 1'
    result += ' --tx-body-file ' + FILE_RAW
    result += ' --out-file ' + FILE_SINGNED
    return result
# res = sing_colateral_transaction()
# print(res)
#endregion

#region TRANSACCIONES
# cd ppbl-course-02/activitis/project-02/plutus-scripts/
POLICYID = 'a10aa40d0ec3fd4c8fa33a2910fb27941ae8b7ad2a5b0c30816f7a20'
TOKEN_S1 = 'City'
POLICYSCRIPT = 'my-minting-script.plutus'
REDEEMERFILE = 'redeemer.json'

def mint_plutus_token_transaction():
    result =  'CLI transaction build'
    result += ' --babbage-era' 
    result += ' --testnet-magic 1' 
    result += ' --tx-in ' + TXHASH  + '#' + TXIX
    result += ' --tx-out ' + SENDER + '+' + str(MIN_SEND) 
    result += '+"' + str(TOKEN3_AMOUNT) + ' ' + POLICYID + '.' + toHex(TOKEN_S1) + '"' 
    result += ' --mint '+'"' + str(TOKEN3_AMOUNT) + ' ' + POLICYID + '.' + toHex(TOKEN_S1) + '"' 
    result += ' --mint-script-file ' + POLICYSCRIPT 
    result += ' --mint-redeemer-file ' + REDEEMERFILE
    result += ' --tx-in-collateral ' + TXHASH2 + '#' + TXIX2
    result += ' --change-address ' + SENDER 
    result += ' --protocol-params-file protocol.json' 
    result += ' --out-file ' + FILE_RAW
    return result
# res = mint_plutus_token_transaction()
# print(res)


def sing_plutus_token_transaction():
    result =  'CLI transaction sign'
    result += ' --signing-key-file ' + TOK_SKEY
    result += ' --testnet-magic 1'
    result += ' --tx-body-file ' + FILE_RAW
    result += ' --out-file ' + FILE_SINGNED
    return result
# res = sing_plutus_token_transaction()
# print(res)

def submit_plutus_token_transaction():
    result =  'CLI transaction submit'
    result += ' --tx-file ' + FILE_SINGNED
    result += ' --testnet-magic 1'
    return result
# res = submit_token_transaction()
# print(res)
#endregion
#endregion

#region TRANSACTION METADTA
mdfile = '/opt/DEV/PLUTUS/ppbl-course-02/activitis/project-02/metadata/metadata.json'
fee = 169153
def mint_metadata_transaction():
    result =  'CLI transaction build'
    result += ' --babbage-era' 
    result += ' --testnet-magic 1' 
    result += ' --tx-in ' + TX['tx']  + '#' + TX['ix']
    result += ' --tx-out ' + SENDER + '+' + str(int(TX['lovelace']) - fee)
    result += ' --metadata-json-file' + ' ' + mdfile
    result += ' --change-address ' + SENDER 
    result += ' --protocol-params-file protocol.json' 
    result += ' --out-file ' + FILE_RAW
    return result
# res = mint_metadata_transaction()
# print(res)
    
def sing_metadata_transaction():
    result =  'CLI transaction sign'
    result += ' --signing-key-file ' + TOK_SKEY
    result += ' --testnet-magic 1'
    result += ' --tx-body-file ' + FILE_RAW
    result += ' --out-file ' + FILE_SINGNED
    return result
res = sing_plutus_token_transaction()
print(res)

def submit_metadata_transaction():
    result =  'CLI transaction submit'
    result += ' --tx-file ' + FILE_SINGNED
    result += ' --testnet-magic 1'
    return result
res = submit_token_transaction()
print(res)

# query_result = subprocess.check_output(res)
# query_result = query_result.strip().splitlines()[2:]



#endregion

def calc_fee():
    result = 'CLI transaction calculate-min-fee'
    result += ' --tx-body-file ' + FILE_RAW
    result += ' --tx-in-count 1'
    result += ' --tx-out-count 2'
    result += ' --witness-count 1'
    result += ' $MAGICT'
    result += ' --protocol-params-file ' + PROTOCOL_FILE
    return result

def make_draft_transaction():
    result = 'CLI transaction build --babbage-era' + MAGICT
    result += ' --tx-in '  + TXHASH + '#' + TXIX
    result += ' --tx-out ' + SENDER + '+' + str(MIN_SEND)
    result += ' --tx-out ' + RECEIVER + '+' + str(MIN_SEND)
    # result += ' --tx-out ' + SENDER + '+' + str(MIN_SEND)
    # result += ' --tx-out ' + SENDER + '+' + str(MIN_SEND)
    # result += ' --change-address '  + SENDER
    result += ' --out-file '        + FILE_RAW
    return result

token_TX = txlist[0][0].decode()
token_IX = txlist[0][1].decode()
tada_min = 3000000
tada_amount = int(txlist[0][2].decode()) + int(txlist[1][2].decode())
token_amount = int(txlist[0][5].decode())
token_ID = txlist[0][6].decode()
fee = 179537 # -179537

# cd ppbl-course-02/activitis/project-02/plutus-scripts/
def make_transaction():
    result = 'CLI transaction build --babbage-era  --testnet-magic 1'
    result += ' --change-address ' + RECEIVER 
    # result += ' --fee ' + str(FEE)
    result += ' --tx-in ' + token_TX + '#' + str(0)
    result += ' --tx-in ' + txlist[1][0].decode() + '#' + str(0)
    result += ' --tx-out ' + SENDER + '+' + str(tada_amount -tada_min -fee) + '+"' + str(token_amount -1) + ' ' + token_ID + '"'
    result += ' --tx-out ' + RECEIVER + '+' + str(tada_min ) + '+"' + str(1) + ' ' + token_ID + '"'
    result += ' --out-file ' + FILE_RAW
    return result



# res = make_transaction()
# print(res)
# res = sing_token_transaction()
# print(res)
# res = submit_token_transaction()
# print(res)


# def make_transaction2():
#     result = 'CLI transaction build --babbage-era  --testnet-magic 1'
#     # result += ' --change-address ' + SENDER 
#     # result += ' --fee ' + str(FEE)
#     result += ' --tx-in ' + TXTOKEN + '#' + TXTOKENIX
#     result += ' --tx-out ' + SENDER + '+' + str(MIN_SEND) + '+"' + str(
#         1) + ' ' + POLICYID + '.' + TOKEN_S1 + '"'
#     result += ' --tx-out ' + RECEIVER + '+' + str(SENDER_AMOUNT - MIN_SEND - FEE) + '+"' + str(
#         1) + ' ' + POLICYID + '.' + TOKEN_S1 + '"'
#     result += ' --out-file ' + FILE_RAW
#     return result

# def sing():
#     result = 'CLI transaction sign'
#     result += ' --signing-key-file ' + SENDER_SKEY
#     # result += ' --signing-key-file ' + POL_SKEY
#     result += MAGICT
#     result += ' --tx-body-file ' + FILE_RAW
#     result += ' --out-file ' + FILE_SINGNED
#     return result


# def submit():
#     # CLI     transaction    submit - -tx - file    toktx.signed  $MAGICT
#     result = 'CLI transaction submit'
#     result += ' --tx-file ' + FILE_SINGNED + MAGICT
#     return result


# def burnign():
#     result = 'CLI transaction build-raw'
#     result += ' --fee ' + str(FEE)
#     result += ' --tx-in ' + TXHASH_TOK + '#' + TXIX
#     result += ' --tx-out ' + RECEIVER + '+' + str(
#         RECEIVER_AMOUNT - FEE) + '+"40 ' + POLICYID + '.' + TOKEN1 + ' + ' + '319 ' \
#               + POLICYID + '.' + TOKEN2 + '"'
#     result += ' --mint=' + '"-1 ' + POLICYID + '.' + TOKEN1 + \
#               ' + ' + '-1 ' + POLICYID + '.' + TOKEN2 + '"'
#     result += ' --minting-script-file policy/policy.script'
#     # result += ' --metadata-json-file tokens.json'
#     result += ' --out-file ' + FILE_RAW
#     return result


# def mint():
#     assets = ['AAA', 'BBB', 'CCC', 'DDD', 'EEE', 'FFF', 'GGG', 'HHH']
#     amounts = [1, 20, 400, 4000, 40000, 200000, 1000000, 5000000]
#     mint = ''
#     for a in assets:
#         mint += str(amounts[assets.index(a)]) + ' ' + POLICYID + '.' + a + ' '

#     result = 'CLI transaction build-raw'
#     result += ' --fee ' + str(FEE)
#     result += ' --tx-in ' + TXHASH_TOK + '#' + TXIX
#     result += ' --tx-out ' + RECEIVER + '+' + str(MIN_SEND) + '+"' + str(
#         TOKEN1_AMOUNT) + ' ' + POLICYID + '.' + TOKEN1 + ' + ' + str(TOKEN2_AMOUNT) \
#               + ' ' + POLICYID + '.' + TOKEN2 + '"'
#     result += ' --mint="' + mint
#     result += ' --minting-script-file ' + FILE_SCRIPT
#     result += ' --out-file ' + FILE_RAW
#     # result += ' --metadata-json-file ' + 'mint.json'
#     return result


# def createPolicyID():
#     # \"$(cardano-cli address key-hash --payment-verification-key-file policy/policy.vkey)\"
#     hash_policyID = '920ffdb246cb66dfdd74e29f76529f75a0a4aaaeebcce12a9c7f44cb'
#     # \"slot\": $(expr $(cardano-cli query tip --mainnet | jq .slot?) + 10000)"
#     # CLI query tip $MAGICT | jq.slot --> 40008254
#     slot = 1000

#     result = {"type": "all",
#               "scripts": [
#                   {
#                       "type": "before",
#                       "slot": slot
#                   },
#                   {
#                       "type": "sig",
#                       "keyHash": hash_policyID
#                   }
#               ]
#               }

#     return result

# print(make_transaction())
# print(make_draft_transaction())
# print(calc_fee())
# print(sing())
# print(submit())
# print(burnign())
# pprint(mint())
# pprint(json.dumps(createPolicyID()))
