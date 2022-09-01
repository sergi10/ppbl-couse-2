# Zero One Game

## Description

- This package contains Plutus source code for Zero One Game project.
  
## Installation

Once you have Plutus development environment, to compile smart contract follow these instructions:
  
- Run `nix-shell` inside `plutus-app` folder
- Change directory to `ppbl-course-02/project-303/Zero One Game`
- Then execute `cabal update` command
  
## Usage

Follow these steps to make smart contract ready for compilation:

### Step 1

Mint an NFT which will work as **State NFT**. Obtain its **TokenName** and **CurrencySymbol** and inside `Game.GameComplier.hs` file replace `stateNftCurrencySymbol` and `stateNftTokenName` values with the values you've obtained.  

### Step 2

Provide 2 wallets if you want to play as both player and obtain its payment public hash to be used as `firstPlayerPKH` and `secondPlayerPKH` in `Game.GameComplier`.

### Step 3

Change values of `gBet`, `gPlayDeadline` and `gRevealDeadline` to the desired values that you want game take effects.

### Step 4

Run `cabal repl` and check there is no error (you can ignore warnings although it is good practice to solve those too)

### Step 5

Run `writeZeroOneGameScript` and get `Right()` as output which indicate **Zero One Game** contract was complied successfully.

## Tooling

There are 2 scripts at `/app` folder for creating correct **Datum** and **Redeemer**, also **Hash of The Answer** for the smart contract, so `cardano-cli` or front-end transactions crafting will be possible.

Inside `/project-303/Zero One Game` folder run:

- To create a **Datum** and **Redeemer** `datumToJSON.hs`
  
```bash
cabal run datumToJSON
```

- To produce hash of answer `createHashOfAnswer.hs`

```bash
cabal run createHashOfAnswer
```

## Note

- All values are just examples.
- All output files will be written to `/Zero One Game/output`
directory.
- Feel free change, optimize and experiment with all codes and scripts, especially try to create different **Datum** and **Redeemer**.

## License

Apache 2.0
