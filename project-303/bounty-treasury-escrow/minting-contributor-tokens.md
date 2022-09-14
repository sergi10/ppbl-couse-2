# Gimbal Bounty Treasury and Escrow (GBTE)
## Pre-Production Instance for PPBL Summer 2022

### Roadmap:
[x] Use a bash script to mint one Contributor Token
[ ] Use a bash script to mint a set of Contributor Tokens
[ ] Create a front end for minting Contributor Tokens with Mesh
[ ] Create a Plutus script for minting Contributor Tokens for holders of PPBLSummer2022
[ ] Create a self-service front end for minting Contributor Tokens.

### Instance Parameters:
In order to compile and use our contract, need to prepare some tokens:
1. For our fungible "Bounty Token", we will use the `tgimbals` that already minted on Pre-Prod with policyId `fb45417ab92a155da3b31a8928c873eb9fd36c62184c736f189d334c`

2. We also need the policyId for a Contributor Access Token. We could use the `PPBLSummer2022` token for this, but then we would not be able to distinguish between the tokens of different Contributors. That is why our Validator Parameters only require a PolicyId for the Contributor Token. We want to allow for the name of the token to be anything, so that we can use the TokenName to track individual contributions.

The Contributor Token policyId is `738ec2c17e3319fa3e3721dbd99f0b31fce1b8006bb57fbd635e3784`. It was made with the following script:
```
{
  "type": "all",
  "scripts":
  [
    {
      "type": "sig",
      "keyHash": "e8daa14577ddd2137417006152fa4fa8361660b2d7011f4a7e36ae35"
    },
    {
      "type": "sig",
      "keyHash": "65295d6feacfc33fe029f51785770d92373e82cde28c3cd8c55a3cd1"
    }
  ]
}
```

Where one `keyHash` represents Signing Key made specifically for this project, and the other is the PKH of the Treasury Issuer wallet. The Treasury Issuer's key is `65295d6feacfc33fe029f51785770d92373e82cde28c3cd8c55a3cd1`.

We will also need to specify a Treasury Issuer. This is represented by the Public Key Hash of the "owner" of a GBTE instance. We can use the `keyHash` above as the `treasuryIssuerPkh` (Note that there are probably better ways to handle Treasury "ownership" - we'll cover this when we get to Contribution stage of this project.)


## Use a bash script to mint one Contributor Token
- see `/scripts/contrib-preprod-minter.sh`