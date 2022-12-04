import json, os, subprocess
from pprint import pprint
import tabview as t
from pycardano import Address, Network, UTxO, Transaction, TransactionInput, MultiAsset, ScriptHash, AssetName, SCRIPT_HASH_SIZE, Asset, Value, DatumHash, RawCBOR, TransactionOutput
from typing import  List

# region  GLOBALS
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

# endregion


# PROCESS TO GET ADDRESSES
# Read wallet address value from payment.addr file
with open(os.path.join(DIR, SENDER_ADDR), 'r') as file:
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
#endregion

def tx2utxos(txs,address)-> List[UTxO]:
    results = txs

    utxos = []

    for result in results:
        tx_in = TransactionInput.from_primitive(
            [result[0].decode(), result[1].decode()]
        )
        amount = result[2].decode()
        lovelace_amount = 0
        multi_assets = MultiAsset()
        if result[5].decode() != 'TxOutDatumNone':
            for h in range(5, len(result) -2, 3):

        # for item in result[3].decode():
            # if item == "lovelace":
            #     lovelace_amount = int(item[4].decode())
            # else:
                # The utxo contains Multi-asset
                # data = bytes.fromhex(result[h+1])
                # data = result[h+1].decode()
                # data = bytes.fromhex(asset)
                # policy_id = ScriptHash(data[:SCRIPT_HASH_SIZE])
                # asset_name = AssetName(data[SCRIPT_HASH_SIZE:])
                info = result[h+1].decode().split('.')
                policy_id= ScriptHash(info[0])
                asset_name = AssetName(info[1])

                if policy_id not in multi_assets:
                    multi_assets[policy_id] = Asset()
                multi_assets[policy_id][asset_name] = int(result[h])

        amount = Value(lovelace_amount, multi_assets)

        # datum_hash = (
        #     DatumHash.from_primitive(result.data_hash)
        #     if result.data_hash and result.inline_datum is None
        #     else None
        # )
        datum_hash = None

        datum = None

        # if hasattr(result, "inline_datum") and result.inline_datum is not None:
        #     datum = RawCBOR(bytes.fromhex(result.inline_datum))

        script = None

        # if (
        #     hasattr(result, "reference_script_hash")
        #     and result.reference_script_hash
        # ):
        #     script = self._get_script(result.reference_script_hash)

        tx_out = TransactionOutput(
            Address.from_primitive(address),
            amount=amount,
            datum_hash=datum_hash,
            datum=datum,
            script=script,
        )
        utxos.append(UTxO(tx_in, tx_out))

    return utxos


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
pprint(tx2dict(txlist))

addres_txs = tx2utxos(txlist, wllt)

wallet_Txs = tx2dict(txlist)

TX = wallet_Txs['transactions'][2]
TXHASH = TX['tx']
TXIX  = TX['ix']
TXHASH = txlist[1][0].decode()
TXIX  = txlist[1][1].decode()
TXHASH2 = txlist[2][0].decode()
TXIX2  = txlist[2][1].decode()

# t.view(txlist)

# def utxos(self, address: str) -> List[UTxO]:
#     results = self.api.address_utxos(address, gather_pages=True)

#     utxos = []

#     for result in results:
#         tx_in = TransactionInput.from_primitive(
#             [result.tx_hash, result.output_index]
#         )
#         amount = result.amount
#         lovelace_amount = 0
#         multi_assets = MultiAsset()
#         for item in amount:
#             if item.unit == "lovelace":
#                 lovelace_amount = int(item.quantity)
#             else:
#                 # The utxo contains Multi-asset
#                 data = bytes.fromhex(item.unit)
#                 policy_id = ScriptHash(data[:SCRIPT_HASH_SIZE])
#                 asset_name = AssetName(data[SCRIPT_HASH_SIZE:])

#                 if policy_id not in multi_assets:
#                     multi_assets[policy_id] = Asset()
#                 multi_assets[policy_id][asset_name] = int(item.quantity)

#         amount = Value(lovelace_amount, multi_assets)

#         datum_hash = (
#             DatumHash.from_primitive(result.data_hash)
#             if result.data_hash and result.inline_datum is None
#             else None
#         )

#         datum = None

#         if hasattr(result, "inline_datum") and result.inline_datum is not None:
#             datum = RawCBOR(bytes.fromhex(result.inline_datum))

#         script = None

#         if (
#             hasattr(result, "reference_script_hash")
#             and result.reference_script_hash
#         ):
#             script = self._get_script(result.reference_script_hash)

#         tx_out = TransactionOutput(
#             Address.from_primitive(address),
#             amount=amount,
#             datum_hash=datum_hash,
#             datum=datum,
#             script=script,
#         )
#         utxos.append(UTxO(tx_in, tx_out))

#     return utxos


#region GRAPHQL
gql = 'https://graphql-api.iohk-preprod.dandelion.link'
# https://github.com/graphql-python/gql

{"adr":"addr_test1vrtnnles3mxk8dy9fcrha8gl5la98hxwx00llc437kkjnhcqsxc8r"}
query = "query utxosbyAddress($adr:String){ \
  utxos(where:{address:{_eq:$adr}}){  \
    address\
    txHash\
    index\
    value\
    tokens{ \
      quantity \
      asset{ \
        assetId \
        assetName\
        decimals\
        description\
        name\
        policyId\
        ticker\
      }\
    }\
  }\
}"