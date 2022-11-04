# Gimbal Bounty Treasury and Escrow (GBTE)

### This serves as a guide for interacting with the existing instance of GBTE using plutus V1 on pre-production network. 

## Pre-Production Instance - Updated 2022-10-25 

## Key Changes:
1. Add parameterized redeemer to Treasury Validator
2. Add Issuer Token to replace Issuer PKH logic.

## Contract Addresses:
- Treasury: `addr_test1wrk2n3ygme5jh05nm668eu26phljpg56pd8lts27j9ucc0qgc0ypz`
- Bounty Escrow: `addr_test1wzyvjgjxy5mr88ny3sm96qatd90fazsj625gxjr8hhrklqsf6ftxl`

## Tokens
- Issuer Token (NFT): `94784b7e88ae2a6732dc5c0f41b3151e5f9719ea513f19cdb9aecfb3`
- Contributor Token (NFT): `738ec2c17e3319fa3e3721dbd99f0b31fce1b8006bb57fbd635e3784`
- Bounty FT: `fb45417ab92a155da3b31a8928c873eb9fd36c62184c736f189d334c`


## What's Missing:
- [ ] A check that TreasuryAction -> BountyDetails matches BountyEscrowDatum.

### Instance Parameters:
In order to use this contract, we need to understand the tokens. For more info see Review [minting-tokens.md](https://gitlab.com/gimbalabs/plutus-pbl-summer-2022/ppbl-course-02/-/blob/master/project-303/bounty-treasury-escrow/minting-tokens.md):

#### 1. NEW WITH THIS UPDATE: Issuer Token
To interact with this instance of the GBTE, you will not need to create an issuer token. The issuer is your course administrator at Gimbalabs! They have already funded the contract and will release the bounty once you have submitted it.

#### 2. Bounty Tokens
In this example we will use the `tGimbal` tokens that already minted on Pre-Prod with policyId `fb45417ab92a155da3b31a8928c873eb9fd36c62184c736f189d334c`. When you create your own instance, you can mint your own fungible token, or continue to use `tGimbal`'s.

#### 3. Contributor Access Tokens
You will need to apply for an access token in Canvas to be able to interact with this contract. Fill out the form [here](https://docs.google.com/forms/d/e/1FAIpQLSffNGQdMqqr3mp1WfxyfexR7BxHXbsbCHIpCnYt2XAM3aYgSQ/viewform)

When the GBTE is compiled it takes the policyId for a Contributor Access Token as a parameter. We could have used the `PPBL2022AccessToken` token for this, but then we would not be able to distinguish between the tokens of different Contributors. That is why our Validator Parameters only require a PolicyId for the Contributor Token. We want to allow for the name of the token to be anything, so that we can use the TokenName to track individual contributions.

In this example, the Contributor Token policyId is `738ec2c17e3319fa3e3721dbd99f0b31fce1b8006bb57fbd635e3784`. Once again, you can use this token (because PPBL students will already have it), or you can mint your Contributor NFTs.

Review [minting-tokens.md](https://gitlab.com/gimbalabs/plutus-pbl-summer-2022/ppbl-course-02/-/blob/master/project-303/bounty-treasury-escrow/minting-tokens.md) for details of how these tokens are minted.

#### Compiling the Contracts
To interact with this instance of the GBTE, you will need properly compiled plutus scripts. You will want to be sure your `Compiler.hs` document is unchanged. The parameters should appear as below. Note that some parameters in the Escrow Contract (`treasuryIssuerPkh`) and the Treasury Contract (`tTreasuryIssuerPkh`) are the same. These parameters have different names to prevent naming collisions in Haskell.

```
writeBountyEscrowScript :: IO (Either (FileError ()) ())
writeBountyEscrowScript = writeValidator "output/example-bounty-escrow-new-preprod.plutus" $ Escrow.validator $ BountyParam
    {
      bountyTokenPolicyId     = "fb45417ab92a155da3b31a8928c873eb9fd36c62184c736f189d334c"
    , bountyTokenName         = "tGimbal"
    , accessTokenPolicyId     = "738ec2c17e3319fa3e3721dbd99f0b31fce1b8006bb57fbd635e3784"
    , treasuryIssuerPolicyId  = "94784b7e88ae2a6732dc5c0f41b3151e5f9719ea513f19cdb9aecfb3"
    }

writeBountyTreasuryScript :: IO (Either (FileError ()) ())
writeBountyTreasuryScript = writeValidator "output/example-bounty-treasury-new-preprod.plutus" $ Treasury.validator $ TreasuryParam
    {
      tAccessTokenPolicyId    = "738ec2c17e3319fa3e3721dbd99f0b31fce1b8006bb57fbd635e3784"
    , tIssuerTokenPolicyId    = "94784b7e88ae2a6732dc5c0f41b3151e5f9719ea513f19cdb9aecfb3"
    , bountyContractHash      = "88c922462536339e648c365d03ab695e9e8a12d2a8834867bdc76f82"
    , tBountyTokenPolicyId    = "fb45417ab92a155da3b31a8928c873eb9fd36c62184c736f189d334c"
    , tBountyTokenName        = "tGimbal"
    }
```
Open the repl and run `writeBountyEscrowScript` and `writeBountyTreasuryScript`.

## Build Contract Addresses:
```
cardano-cli address build --testnet-magic 1 --payment-script-file example-bounty-treasury-new-preprod.plutus --out-file example-bounty-treasury-new-preprod.addr
cardano-cli address build --testnet-magic 1 --payment-script-file example-bounty-escrow-new-preprod.plutus --out-file example-bounty-escrow-new-preprod.addr
```
These addresses are provided above, but this is good practice, and if the addresses you generate don't match those given at the beggining of this doc, then you need to double check that your parameters match the above.

## Preparing Datum and Redeemers

Now we are almost ready to test the contract, but first, we'll have to prepare some Datum and Redeemer files. They are a bit more complex than what we used in the Faucet Mini-Project. For this excercise, we will only need the types `TreasuryDatum` and `TreasuryAction`, but the other types are explained here as well and will be useful for compiling our own contracts later.

### Treasury Contract
- Datum type: `TreasuryDatum` - is a placeholder for now, but does have a specified type. For this example, we can use the file `ppbl-course-02/project-303/bounty-treasury-escrow/datum-and-redeemers/TreasuryDatumExample01.json` as the Treasury Datum.
- Redeemer type: `TreasuryAction` - this Record type contains a constructor, then and data matching `Bounty Details`. 
### Escrow Contract
- Datum type: `BountyEscrowDatum` - this Record type matches `BountyDetails`.
- Redeemer type: `BountyAction` - is a simple Action, which follows a pattern you'll want to get to know in the next section below

### Datum and Redeemer Contents

#### Bounty Details

Whether it's the TreasuryAction or BountyEscrowDatum, here is what we are working with:

```
data BountyDetails = BountyDetails
  { contributorPkh      :: !PubKeyHash
  , lovelaceAmount      :: !Integer
  , tokenAmount         :: !Integer
  , expirationTime      :: !POSIXTime
  } deriving (Pr.Eq, Pr.Ord, Show, Generic, ToJSON, FromJSON, ToSchema)
```

Each BountyDetails specifies a Bounty by recording the following:
1. The PubKeyHash Contributor (who will be interacting with the Treasury Contract, and therefore providing their keyhash)
2. A number of Lovelace to be locked in the Bounty Commitment UTxO
3. A number of tgimbal (or in your own instance another bounty token) to be locked in the Bounty Commitment UTxO
4. An expiration POSIXTime for the Bounty, which allows the Issuer to Cancel the bounty commitment after a certain date. 

### About POSIXTime:
- [Calculate UNIX time here](https://www.epochconverter.com/)
- POSIXTtime is equal to UNIX time in milliseconds. So multiply "Unix Time" by 1000.

#### Creating TreasuryAction and BountyEscrowDatum:
Both TreasuryAction and BountyEscrowDatum are built on the `BountyDetails` data type. Some utilities are provided to build Datum and Redeemer types, but take a look at this output. It's a JSON object:

```
{
    "constructor": 0,
    "fields": [
        {
            "bytes": "c1d812436253485ac6226a55e93903e85f64c613b9e83602f9ca3d56"
        },
        {
            "int": 7500000
        },
        {
            "int": 200
        },
        {
            "int": 1669388578000
        }
    ]
}
```

The first `bytes` string is our Contributor PubKeyHash, the next two `int`s are the lovelace and tgimbal amounts, and the last `int` is POSIXTime. This is all that is needed for BountyEscrowDatum. A TreasuryAction will contain the same information but wrapped in another `Constructor` with all this content in the `fields` heading.

#### BountyAction:
Redeemer Actions are a bit different. In Types.hs, we map each section to an index:

```
data BountyAction = Cancel | Update | Distribute
  deriving Show

PlutusTx.makeIsDataIndexed ''BountyAction [('Cancel, 0), ('Update, 1), ('Distribute, 2)]
PlutusTx.makeLift ''BountyAction
```
#### Creating BountyAction:
To represent these "Actions" in Transactions, we can create three simple Redeemer files:

`Cancel.json`
```
{"constructor":0,"fields":[]}
```

`Update.json`
```
{"constructor":1,"fields":[]}
```

`Distribute.json`
```
{"constructor":2,"fields":[]}
```

#### TreasuryDatum
Finally, `TreasuryDatum` takes a `issuerTokenName`, which is the name of the issuer's issuer token in hex, and a `bountyCount` which is an integer that is currently unused in the contract, but could be used by our front-end template to keep count of the number of bounties issued. It looks like this:
```
data TreasuryDatum = TreasuryDatum
  { bountyCount     :: !Integer
  , issuerTokenName :: !TokenName
  } deriving (Pr.Eq, Pr.Ord, Show, Generic)
```

## Let's Build Commit to a Bounty:
- Treasury: `addr_test1wrk2n3ygme5jh05nm668eu26phljpg56pd8lts27j9ucc0qgc0ypz`
- Bounty Escrow: `addr_test1wzyvjgjxy5mr88ny3sm96qatd90fazsj625gxjr8hhrklqsf6ftxl`

### Contributor Commits to Bounty
- See [/scripts/02-contributor-commits-to-bounty.sh](https://gitlab.com/gimbalabs/plutus-pbl-summer-2022/ppbl-course-02/-/blob/master/project-303/bounty-treasury-escrow/scripts/02-contributor-commits-to-bounty.sh)

