# Bounty Escrow

## Contents:
This document provides basic instruction for compling and using a unique instance of the GBTE. Further documentation includes:
- [Using GBTE on Pre-Production](https://gitlab.com/gimbalabs/plutus-pbl-summer-2022/ppbl-course-02/-/blob/master/project-303/bounty-treasury-escrow/using-preprod-instance.md) - for interacting with the Gimbalabs instance of the GBTE on pre-production network.
- [Preparing Tokens](https://gitlab.com/gimbalabs/plutus-pbl-summer-2022/ppbl-course-02/-/blob/master/project-303/bounty-treasury-escrow/minting-contributor-tokens.md) - for details on the types of tokens to produce for an instance of the GBTE.
- [Scripts for building each transaction](https://gitlab.com/gimbalabs/plutus-pbl-summer-2022/ppbl-course-02/-/tree/master/project-303/bounty-treasury-escrow/scripts) - To help understand how this dapp works, and ease of building transactions,

## Description
- This package contains Plutus source code for Bounty and Escrow Project.
- This is a bounty campaign that is been managed by PPBL students which they will create task for other students in form of bounty and then reward participant.
- Running bounty escrow campaign on blockchain have these advantages:
    1. If a participant try to use ill-practice behavior for any bounty, the participant will be disqualified and their funds which was locked inside smart contract will be taken by campaign management.
    2. The rules set out in these bounties are not fixed and according to the bounty it is flexible and changeable and the participants will be rewarded accordingly and no negotiation will be needed.
    3. Commit to a bounty is handled by on-chain action, therefore the campaign can be used worldwide and as transparent as possible.
    4. Right now, submission acceptance and approval of successful bounty is implemented off-chain, which means participants must contact campaign management in order to get rewarded. [Gimbalabs implementation of Bounty Escrow off-chain code (front-end)](https://gitlab.com/gimbalabs/gimbal-bounty-treasury-and-escrow/gimbal-tracker-v1)
    5. Credential (access) token logic is implemented inside this project therefore only participants who authenticated and authorized by campaign management are able to commit to a bounty, that said, campaigns management is responsible for distributing its credential token.
    6. Since 2 smart contracts (Treasury and Escrow) was implemented, the result of this design pattern offer more security and less unwanted result(broken bounty) than normal bounty escrow smart contracts.

- There are lot more advantages for such smart contract usage, so we strongly encourage students to change and improve the business logic of these smart contracts.

## Installation

Once you have Plutus development environment, to compile the smart contracts follow these instructions:

- Run `nix-shell` inside `plutus-app` folder
- Change directory to `ppbl-course-02/project-303/Bounty Escrow`
- Then execute `cabal update` command

## Usage

Follow these steps to make smart contracts ready for compilation:

### Step 1

You will need to mint a series of tokens to use with this contract. If you need further details check out the guide to [GBTE Tokens](https://gitlab.com/gimbalabs/plutus-pbl-summer-2022/ppbl-course-02/-/blob/master/project-303/bounty-treasury-escrow/minting-contributor-tokens.md) . The tokens you will need are:
- Issuer token(s) - An NFT. Minimum one but if you make multiples, be sure they have distinct names to identify each issuer. These are requireed to issue bounties
- Contributor token(s) - similar to the above issuer token, but will need a seperate, and distinct policyID. Required to commit to bounties.
- Bounty Tokens - A fungible token. You will want to mint many of these as they are used for paying out bounties, but keep in mind the **Hard Limit**..

Values from these tokens will be used to fill out the following variables in `Compiler.hs`:

- `bountyTokenPolicyId `    
- `bountyTokenName `        
- `accessTokenPolicyId`     
- `treasuryIssuerPolicyId`  
- `tAccessTokenPolicyId`   
- `tIssuerTokenPolicyId`     
- `tBountyTokenPolicyId`  
- `tBountyTokenName`  

### Step 2

Once these values are provided (`bountyContractHash` will still be empty), run `cabal repl`, and check there is no error (you can ignore warnings although it is good practice to solve those too)

### Step 3

Run `writeBountyEscrowScript` and get `Right()` as output which indicate **Bounty Escrow** contract was complied successfully.

### Step 4

Obtain contract address and contract hash (policy ID) by running this command

```
cardano-cli transaction policyid --script-file example-bounty-escrow-new-preprod.plutus
> 88c922462536339e648c365d03ab695e9e8a12d2a8834867bdc76f82
```

### Step 5

Provide this value for `writeBountyTreasuryScript` function, and then compile treasury contract by running `writeBountyTreasuryScript` inside repl and get `Right()` as output which indicate **Bounty Treasury** contract was complied successfully.

## Step 6

Generate the plutus script addresses using:
```
cardano-cli address build --testnet-magic 1 --payment-script-file example-bounty-treasury-new-preprod.plutus --out-file bounty-treasury.addr
cardano-cli address build --testnet-magic 1 --payment-script-file example-bounty-escrow-new-preprod.plutus --out-file bounty-escrow.addr
```

## Design patterns

- After obtaining treasury contract address, create bounty (task) with correct Datum and send bounty detail as metadata inside a that transaction.

- Participants who obtained credential (access) token will be able to consume eUTxO at **Treasury** contract and produce output at **Bounty** contract address, which indicate they are committed to the bounty.

- Bounty campaign manager will consume eUTxO at **Bounty** contract and send the reward to participants who successfully completed the bounty or send the reward back to **Treasury** contract upon unsuccessful completion of bounty.

## Tooling

There are 2 scripts at `/app` folder for creating correct **Datum** or **Redeemer** for these smart contract, so `cardano-cli` or front-end transactions crafting will be possible. The outcome of these scripts are same but different in obtaining values, one has hardcoded values and other one gets value from command line.

Feel free to use either and choose one to optimize and create different **Datum** and **Redeemer** with it. Note that the values are just examples.

Inside `/project-303/bounty-treasury-escrow` folder run:

- `dataToJSON-Hardcoded.hs`

```bash
cabal run dataToJSON-Hardcoded
```

- `dataToJSON-WithArg.hs`

```bash
cabal run dataToJSON-WithArg "22117fbd0f86a213ae4f4d824cd0d38eea29e49764ae22f5f50ba3d3" "e02f8dd57e378ee673d5bf3bf7228382f131b1767d588a79cde2726a" 20 25000 1651252695000
```
## Building Transactions:

### 1. Issuer Locks Tokens at Treasury
- See [/scripts/01-issuer-funds-treasury.sh](https://gitlab.com/gimbalabs/plutus-pbl-summer-2022/ppbl-course-02/-/blob/master/project-303/bounty-treasury-escrow/scripts/01-issuer-funds-treasury.sh)

### 2. Contributor Commits to Bounty
- See [/scripts/02-contributor-commits-to-bounty.sh](https://gitlab.com/gimbalabs/plutus-pbl-summer-2022/ppbl-course-02/-/blob/master/project-303/bounty-treasury-escrow/scripts/02-contributor-commits-to-bounty.sh)

### 3a. Issuer Distributes Bounty
- See [/scripts/03-issuer-distributes-bounty.sh](https://gitlab.com/gimbalabs/plutus-pbl-summer-2022/ppbl-course-02/-/blob/master/project-303/bounty-treasury-escrow/scripts/03-issuer-distributes-bounty.sh)

## Note

- All values are just examples.
- All output files will be written to `/bounty-treasury-escrow/output` directory.
- Feel free change, optimize and experiment with all codes and scripts, especially try to create different **Datum** and **Redeemer**.

## License

Apache 2.0

