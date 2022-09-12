# Zero One Game

## Description

- This package contains Plutus source code for Zero One Game project.
- After following **Installation** and **Preparation** steps, open **`Game Instruction.md`** file to know, the game logic and how to play it.
  
## Installation

Once you have Plutus development environment, to compile the smart contract follow these instructions:
  
- Run `nix-shell` inside `plutus-app` folder.
- Change directory to `ppbl-course-02/project-303/Zero One Game` folder.
- Then execute `cabal update` command.
  
## Preparation

Follow these steps to make smart contract ready for compilation:

### Step 1

Mint an NFT or a token which it will work as **State NFT**, obtain its **TokenName** and **CurrencySymbol** and inside `Game.GameComplier.hs` file replace `nftCurrencySymbol` and `nftTokenName` values with the values you've obtained.  

- Command to mint **State NFT**

```sh
MAGIC=--testnet-magic 1
TXIN=
FIRST_PLAYER=
POLICY_ID=
TOKEN_NAME=$(echo -n "<TOKEN_NAME>" | xxd -p)
POLICY_SCRIPT=
```

```sh
cardano-cli transaction build \
--alonzo-era \
$MAGIC \
--tx-in $TXIN \
--tx-out $(cat $FIRST_PLAYER.addr)+2000000+"1 $POLICY_ID.$TOKEN_NAME" \
--change-address $(cat $FIRST_PLAYER.addr) \
--mint "1 $POLICY_ID.$TOKEN_NAME" \
--mint-script-file $POLICY_SCRIPT \
--out-file tx.unsigned

cardano-cli transaction sign \
--tx-body-file tx.unsigned \
--signing-key-file $FIRST_PLAYER.skey \
--out-file tx.signed

cardano-cli transaction submit $MAGIC --tx-file tx.signed
```

### Step 2

Provide 2 wallets (if you want to play as both player) and obtain its payment public key hash to be used as `firstPlayerPaymentPKH` and `secondPlayerPaymentPKH` inside `Game.GameComplier`.

### Step 3

Create staking credentials and obtain its stake public key hash by following these commands.

```bash
 cardano-cli stake-address key-gen --verification-key-file <WALLET_NAME>.svkey --signing-key-file <WALLET_NAME>.sskey
```

```bash
cardano-cli stake-address key-hash --stake-verification-key-file <FILE_NAME>.svkey
```

Then replace values of `firstPlayerStakePKH` and `secondPlayerStakePKH` inside `Game.GameComplier`.

### Step 4

Create **Shelly Era** address for players by executing this command:

```bash
cardano-cli address build --payment-verification-key-file <WALLET_NAME>.vkey --stake-verification-key-file <WALLET_NAME>.svkey --testnet-magic 1 --out-file <WALLET_NAME>.saddr
```

### Step 5

Change values of `gameBetAmount`, `gamePlayDeadline` and `firstPlayerRevealDeadline` to desired values that you want game take effects.

### Step 6

Run `cabal repl` and check there is no error (you can ignore warnings although it is good practice to solve those too)

### Step 7

Run `writeZeroOneGameScript` and get `Right()` as output which indicate **Zero One Game** contract was complied successfully.

## Tooling

There are 5 scripts at `/app` folder:

- `dataToJSON.hs` creates **Datums** and **Redeemers** for the game.
- `createHashOfAnswer.hs` shows the actual `BuiltinByteString` of the **Hash of The Answer**.
- `addressInfo.hs` shows `Address` type information.
- `posixTimeToSlot.hs` converts `POSIXTime` to `Slot` number for **Pre-Production** testnet.
- `slotToPOSIXTime.hs` converts `Slot` number to `POSIXTime` for **Pre-Production** testnet.

In order to run those script go to `/project-303/Zero One Game` folder and run:
  
```bash
cabal run <NAME_OF_THE_SCRIPT>
```

## Note

- All existing values are just examples.
- All output files will be written at `/Zero One Game/output`
directory.
- `output` folder is consist of example redeemers and datums, so empty it first before creating your own datums and redeemers.
- Feel free to change, optimize and experiment with all codes and scripts, especially try to create different **Datum** and **Redeemer**.

## License

Apache 2.0
