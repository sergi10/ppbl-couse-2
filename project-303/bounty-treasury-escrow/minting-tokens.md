# Gimbal Bounty Treasury and Escrow (GBTE)
## Pre-Production Instance for PPBL 2022

### This doc describes the types of tokens used in an instance of the GBTE.

### Roadmap:
- [x] Use a bash script to mint one Contributor Token
- [ ] Use a bash script to mint a set of Contributor Tokens
- [ ] Create a front end for minting Contributor Tokens with Mesh
- [ ] Create a Plutus script for minting Contributor Tokens for holders of PPBLSummer2022
- [ ] Create a self-service front end for minting Contributor Tokens.

### Types of Tokens used by the GBTElmdb

#### 1. Contributor Token
The first type of token is known as the Contributor Token, and we will need it's policyId to spin up an instance of GBTE. This token allows holders to commit to bounties. We could use the `PPBLSummer2022` token for this, but then we would not be able to distinguish between the tokens of different Contributors. That is why our Validator Parameters only require a PolicyId for the Contributor Token. We want to allow for the name of the token to be anything, so that we can use the TokenName to track individual contributions.

As an example, in the Gimbalabs pre-roduction plutus v1 instance, the Contributor Token policyId is `738ec2c17e3319fa3e3721dbd99f0b31fce1b8006bb57fbd635e3784`. It was made with the following script:
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

#### 2. NEW WITH THIS UPDATE: Issuer Token
In a similar manner. The GBTE uses Issuer Tokens to control who can issue bounties. The `PolicyID` of this token will be a parameter of the contract and should be distinct from the other tokens listed in this document. A user can issue bounties only if they have a token with this policyID. If multiple tokens (for multiple users) are minted, they should each have a different token name.

As an example, one way to produce a different PolicyID would be to use the the time you spin up the instance as a prameter in the script:
```
{
  "type": "all",
  "scripts":
  [
    {
      "type": "after",
      "slot": 11214053
    },
    {
      "type": "sig",
      "keyHash": "65295d6feacfc33fe029f51785770d92373e82cde28c3cd8c55a3cd1"
    }
  ]
}
```
This is one example of how to use the same wallet to produce a different policyID

#### 3. Boutny Tokens
Lastly ,we also need to mint some bounty tokens. These will be fungible tokens given along with Ada for completing a bounty:
- We could use tGimbals for this. These can be obtained from the faucet acessible through the [ppbl-front-end-template](https://gitlab.com/gimbalabs/plutus-pbl-summer-2022/ppbl-front-end-template)
- Another option would be to use a simple, signature only policy script like this one:
```
{
  "type": "sig",
  "keyHash": "65295d6feacfc33fe029f51785770d92373e82cde28c3cd8c55a3cd1"
}
```

### Minting Tokens

To mint these tokens, you hae a few options

#### 1. Mint the tokens manually
- By this point in the course you should be capable of creating all of these tokens using the `cardano-cli`

#### 2. Use the included bash script
- Using `00-preprod-minter.sh` you can mint all the tokens needed to spin up an instance of GBTE.
