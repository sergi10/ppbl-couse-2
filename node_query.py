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

DIR = '/opt/DEV/PLUTUS/tools/preprod-wallets'
TOKEN_FOLDER = '/token-native-script'
CARDANO_CLI_PATH = '/opt/adanode/bin/cardano-cli'
MAGICT = ' --testnet-magic 1 '

# SENDER = 'addr_test1vrtnnles3mxk8dy9fcrha8gl5la98hxwx00llc437kkjnhcqsxc8r'  # 'preprod1.addr'
SENDER_ADDR = 'preprod1.addr' 
SENDER_SKEY = 'preprod1.skey'
# RECEIVER = 'addr_test1vqlj4gyuhs5y6s4y350r0hhsgevfw46wl2hyhnssvnmjxqg8c90rj' # preprod2.addr
RECEIVERR_ADDR = 'preprod2.addr' 
# RECEIVER2 ='addr1qx06ajxagpv8cq8uhun8y45d3vgscjpxaukxus35wc38pk7gzdza8fumastcs6j55660dqdgvs3mdn03cyrp8h0td0csrvpslz' # GameChanger.addr
TXHASH = 'd1fdf7de73288ce9db78926ab7b716ecab63f31cddc13f35d8636670ac04a02d'
# TXHASH_TOK = '37008dfb97d76f1764a380b64968e3cf5734e0075ab42b7edc91c50ece8bea59'
# TXHASH_LOV = 'abf44cbf6abc68c929a33fbd6dbf1839256e7cd868b04c00b9c12c87bcc068e5'
# 'acfa9843ad0d11baa5f5e137e60f6c298569f1b59008e8b54d7c232c652da258'
TXHASH
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
with open(os.path.join(DIR, RECEIVERR_ADDR), 'r') as file:
    RECEIVERR = file.read()
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
result = []
totalLovelaceRecv = 0
for row in utxoTableRows:
    line = row.split()
    # line = row.strip().split()
    # line = list(line)
    result.append(line)
    totalLovelaceRecv +=  int(line[2])
    # print(row) #, sep=' - ', end='')
    # for elem in row:
    #     print(elem) #, sep=' - ', end='')

TXHASH = result[0][0]
TXIX  = result[0][1]
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

def mint_token_transaction():
    result =     'CLI transaction build'
    result +=    ' --babbage-era' 
    result +=    ' --testnet-magic 1' 
    # result +=    ' --fee ' + str(FEE) 
    result +=    ' --tx-in ' + str(TXHASH) + '#' + str(TXIX) 
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
res = mint_token_transaction()
print(res)

def sing_token_transaction():
    result =  'CLI transaction sign'
    result += ' --signing-key-file ' + TOK_SKEY
    result += ' --testnet-magic 1'
    result += ' --tx-body-file ' + FILE_RAW
    result += ' --out-file ' + FILE_SINGNED
    return result
res = sing_token_transaction()
print(res)

def submit_token_transaction():
    result =  'CLI transaction submit'
    result += ' --tx-file ' + FILE_SINGNED
    result += ' --testnet-magic 1'
    return result
res = submit_token_transaction()
print(res)

#endregion

def make_token_transaction():
    result = 'CLI transaction build-raw'
    result += ' --fee ' + str(FEE)
    result += ' --tx-in ' + TXHASH + '#' + TXIX
    result += ' --tx-out ' + SENDER + '+' + str(MIN_SEND) + '+"' + str(
        TOKEN_SEND) + ' ' + POLICYID + '.' + TOKEN1 + ' + ' + str(TOKEN_SEND) \
              + ' ' + POLICYID + '.' + TOKEN2 + '"'
    result += ' --tx-out ' + SENDER + '+' + str(SENDER_AMOUNT - MIN_SEND - FEE) + '+"' + str(
        TOKEN1_AMOUNT - TOKEN_SEND) + ' ' + POLICYID + '.' + TOKEN1 + \
              ' + ' + str(TOKEN2_AMOUNT - TOKEN_SEND) + ' ' + POLICYID + '.' + TOKEN2 + '"'
    result += ' --out-file ' + FILE_RAW
    return result

def make_draft_transaction():
    result = 'CLI transaction build --babbage-era' + MAGICT
    result += ' --tx-in '  + TXHASH + '#' + TXIX
    result += ' --tx-out ' + SENDER + '+' + str(MIN_SEND)
    result += ' --tx-out ' + RECEIVER2 + '+' + str(MIN_SEND)
    # result += ' --tx-out ' + SENDER + '+' + str(MIN_SEND)
    # result += ' --tx-out ' + SENDER + '+' + str(MIN_SEND)
    # result += ' --change-address '  + SENDER
    result += ' --out-file '        + FILE_RAW
    return result


def calc_fee():
    result = 'CLI transaction calculate-min-fee'
    result += ' --tx-body-file ' + FILE_RAW
    result += ' --tx-in-count 1'
    result += ' --tx-out-count 2'
    result += ' --witness-count 1'
    result += ' $MAGICT'
    result += ' --protocol-params-file ' + PROTOCOL_FILE
    return result


def make_transaction():
    result = 'CLI transaction build-raw --babbage-era ' + MAGICT
    result += ' --fee ' + str(FEE)
    result += ' --tx-in ' + TXHASH + '#' + TXIX
    result += ' --tx-out ' + SENDER + '+' + str(MIN_SEND) + '+"' + str(
        TOKEN_SEND) + ' ' + POLICYID + '.' + TOKEN1 + ' + ' + str(TOKEN_SEND) \
              + ' ' + POLICYID + '.' + TOKEN2 + '"'
    result += ' --tx-out ' + SENDER + '+' + str(SENDER_AMOUNT - MIN_SEND - FEE) + '+"' + str(
        TOKEN1_AMOUNT - TOKEN_SEND) + ' ' + POLICYID + '.' + TOKEN1 + \
              ' + ' + str(TOKEN2_AMOUNT - TOKEN_SEND) + ' ' + POLICYID + '.' + TOKEN2 + '"'
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


# print(make_transaction())
# print(make_draft_transaction())
# print(calc_fee())
# print(sing())
# print(submit())
# print(burnign())
# pprint(mint())
# pprint(json.dumps(createPolicyID()))
