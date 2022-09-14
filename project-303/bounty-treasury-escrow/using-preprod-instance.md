# Gimbal Bounty Treasury and Escrow (GBTE)
## Pre-Production Instance for PPBL Summer 2022

### To Do:
- [ ] At Live Coding: Show the relationship between `BountyDetails` in `BountyTreasury.hs` and `BountyEscrowDatum` in `BountyEscrow.hs`. What logic is enabled by using each of these?

### Instance Parameters:
In order to compile and use our contract, we need to prepare some tokens:

#### 1. Bounty Tokens
In this example we will use the `tgimbal` tokens that already minted on Pre-Prod with policyId `fb45417ab92a155da3b31a8928c873eb9fd36c62184c736f189d334c`. When you create your own instance, you can mint your own fungible token, or continue to use `tgimbal`'s.

#### 2. Contributor Access Tokens
We also need the policyId for a Contributor Access Token. We could use the `PPBLSummer2022` token for this, but then we would not be able to distinguish between the tokens of different Contributors. That is why our Validator Parameters only require a PolicyId for the Contributor Token. We want to allow for the name of the token to be anything, so that we can use the TokenName to track individual contributions.

In this example, the Contributor Token policyId is `738ec2c17e3319fa3e3721dbd99f0b31fce1b8006bb57fbd635e3784`. Once again, you can use this token (because PPBL students will already have it), or you can mint your Contributor NFTs.

Review [minting-contributor-tokens.md](https://gitlab.com/gimbalabs/plutus-pbl-summer-2022/ppbl-course-02/-/blob/master/project-303/bounty-treasury-escrow/minting-contributor-tokens.md) for details of how these tokens are minted.

#### Compiling the Contracts
So you will have something like this, maybe with different parameters. The one parameter you must change is the Treasury Issuer's PubKeyHash. Note that this is a parameter of both the Escrow Contract (`treasuryIssuerPkh`) and the Treasury Contract (`tTreasuryIssuerPkh`). These parameters have different names to prevent naming collisions in Haskell.

```
writeBountyEscrowScript :: IO (Either (FileError ()) ())
writeBountyEscrowScript = writeValidator "output/bounty-escrow.plutus" $ Escrow.BountyEscrow.validator $ BountyParam
    {
      bountyTokenPolicyId = "fb45417ab92a155da3b31a8928c873eb9fd36c62184c736f189d334c"
    , bountyTokenName     = "tgimbal"
    , accessTokenPolicyId = "738ec2c17e3319fa3e3721dbd99f0b31fce1b8006bb57fbd635e3784"
    , treasuryIssuerPkh   = "65295d6feacfc33fe029f51785770d92373e82cde28c3cd8c55a3cd1"
    }

writeBountyTreasuryScript :: IO (Either (FileError ()) ())
writeBountyTreasuryScript = writeValidator "output/bounty-treasury.plutus" $ Escrow.BountyTreasury.validator $ TreasuryParam
    {
      tAccessTokenPolicyId = "738ec2c17e3319fa3e3721dbd99f0b31fce1b8006bb57fbd635e3784"
    , bountyContractHash   = ""
    , tBountyTokenPolicyId = "fb45417ab92a155da3b31a8928c873eb9fd36c62184c736f189d334c"
    , tBountyTokenName     = "tgimbal"
    , tTreasuryIssuerPkh   = "65295d6feacfc33fe029f51785770d92373e82cde28c3cd8c55a3cd1"
    }
```
But there is still one parameter missing in our Treasury compiler. We need to reference the Bounty Escrow Contract as a parameter in the Bounty Treasury Contract. First, we must compile the `bounty-escrow.plutus` script. The project's `cabal repl` will load without error if you leave empty strings for any of these parameters. So first, open the repl and run `writeBountyEscrowScript`.

With the output, use the following step to get the `bountyContractHash`:

```
cardano-cli transaction policyid --script-file bounty-escrow.plutus
> 3fceb0fdad63e0e54488da98b9e87c804b148a617af80aa1ad50fdea
```
Yes, this looks like we are trying to make PolicyID, that could be used to mint tokens. The same function is used to derive other sorts of script hashes. Interesting, right?

I'll use `3fceb0fdad63e0e54488da98b9e87c804b148a617af80aa1ad50fdea` as the `bountyContractHash`.

```
writeBountyTreasuryScript :: IO (Either (FileError ()) ())
writeBountyTreasuryScript = writeValidator "output/bounty-treasury.plutus" $ Escrow.BountyTreasury.validator $ TreasuryParam
    {
      tAccessTokenPolicyId = "738ec2c17e3319fa3e3721dbd99f0b31fce1b8006bb57fbd635e3784"
    , bountyContractHash   = "3fceb0fdad63e0e54488da98b9e87c804b148a617af80aa1ad50fdea"
    , tBountyTokenPolicyId = "fb45417ab92a155da3b31a8928c873eb9fd36c62184c736f189d334c"
    , tBountyTokenName     = "tgimbal"
    , tTreasuryIssuerPkh   = "65295d6feacfc33fe029f51785770d92373e82cde28c3cd8c55a3cd1"
    }
```

Reload the repl with this new parameter in place, and compile the Treasury script.

## Build Contract Addresses:
```
cardano-cli address build --testnet-magic 1 --payment-script-file jd-bounty-treasury.plutus --out-file jd-bounty-treasury.addr
cardano-cli address build --testnet-magic 1 --payment-script-file jd-bounty-escrow.plutus --out-file jd-bounty-escrow.addr
```

## Preparing Datum and Redeemers

Now we are almost ready to test the contract, but first, we'll have to prepare some Datum and Redeemer files. They are a bit more complex than what we used in the Faucet Mini-Project.

### Treasury Contract
- Datum type: `WithdrawalDatum` - is a placeholder for now, but does have a specified type.
- Redeemer type: `BountyDetails` - this Record type matches BountyEscrowDatum

### Escrow Contract
- Datum type: `BountyEscrowDatum` - this Record type matches BountyDetails
- Redeemer type: `BountyAction` - is a simple Action, which follows a pattern you'll want to know.

Whether it's the BountyDetails or BountyEscrowDatum, here is what we are working with:

```
data BountyEscrowDatum = BountyEscrowDatum
  { bedIssuerPkh           :: !PubKeyHash
  , bedContributorPkh      :: !PubKeyHash
  , bedLovelaceAmount      :: !Integer
  , bedTokenAmount         :: !Integer
  , bedExpirationTime      :: !POSIXTime
  } deriving (Pr.Eq, Pr.Ord, Show, Generic, ToJSON, FromJSON, ToSchema)
```

Each BountyEscrowDatum specifies a Bounty by recording the following:
1. the PubKeyHash of the Issuer (which is an instance parameter)
2. the PubKeyHash Contributor (who will be interacting with the Treasury Contract, and therefore providing their keyhash)
3. A number of Lovelace to be locked in the Bounty Commitment UTxO
4. A number of tgimbal to be locked in the Bounty Commitment UTxO
5. An expiration POSIXTime for the Bounty, which allows the Issuer to Cancel the bounty commitment after a certain date.

### About POSIXTime:
- [Calculate UNIX time here](https://www.epochconverter.com/)
- POSIXTtime is equal to UNIX time in milliseconds. So multiply "Unix Time" by 1000.

### Creating Datum and Redeemer:

#### BountyDetails and BountyEscrowDatum:
Some utilities are provided to build Datum and Redeemer types. But take a look at this output. It's a JSON object:

```
{
    "constructor": 0,
    "fields": [
        {
            "bytes": "22117fbd0f86a213ae4f4d824cd0d38eea29e49764ae22f5f50ba3d3"
        },
        {
            "bytes": "e02f8dd57e378ee673d5bf3bf7228382f131b1767d588a79cde2726a"
        },
        {
            "int": 20
        },
        {
            "int": 25000
        },
        {
            "int": 1651252695000
        }
    ]
}
```

The first two `bytes` strings are our two PubKeyHashes, the next to `int`s are the lovelace and tgimbal amounts, and the last `int` is POSIXTime.

#### BountyAction:
Redeemer Actions are a bit different. In BountyTypes.hs, we map each action to an index:

```
data BountyAction = Cancel | Update | Distribute
  deriving Show

PlutusTx.makeIsDataIndexed ''BountyAction [('Cancel, 0), ('Update, 1), ('Distribute, 2)]
PlutusTx.makeLift ''BountyAction
```

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

#### WithdrawalDatum
Finally, `WithdrawalDatum` takes a treasuryKey, which is the PubKeyHash of the Isser, and a `bountyCount` which is an integer that is currently unused in teh contract, but could be used by our front-end template to keep count of the number of bounties issued. It looks like this:
```
data WithdrawalDatum = WithdrawalDatum
  { bountyCount     :: !Integer
  , treasuryKey     :: !PubKeyHash
  } deriving (Pr.Eq, Pr.Ord, Show, Generic, ToJSON, FromJSON, ToSchema)
```


## Let's Build Some Transactions:
- Treasury Contract Address: addr_test1wpv67hsrsa6ld9xh4lry98x5uxjmqduvwz20nkneh65a3mgae8qes
- Escrow Contract Address: addr_test1wqluav8a4437pe2y3rdf3w0g0jqyk9y2v9a0sz4p44g0m6skh0uwh

### 1. Issuer Locks Tokens at Treasury
- See [/scripts/01-issuer-funds-treasury.sh](https://gitlab.com/gimbalabs/plutus-pbl-summer-2022/ppbl-course-02/-/blob/master/project-303/bounty-treasury-escrow/scripts/01-issuer-funds-treasury.sh)

### 2. Contributor Commits to Bounty
- See [/scripts/02-contributor-commits-to-bounty.sh](https://gitlab.com/gimbalabs/plutus-pbl-summer-2022/ppbl-course-02/-/blob/master/project-303/bounty-treasury-escrow/scripts/02-contributor-commits-to-bounty.sh)

### 3a. Issuer Distributes Bounty
- See [/scripts/03-issuer-distributes-bounty.sh](https://gitlab.com/gimbalabs/plutus-pbl-summer-2022/ppbl-course-02/-/blob/master/project-303/bounty-treasury-escrow/scripts/03-issuer-distributes-bounty.sh)

### 3b. Issuer Updates Bounty

### 3c. Issuer Cancels Bounty (after deadline)
