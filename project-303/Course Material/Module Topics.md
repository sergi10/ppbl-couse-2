# Module Topics

## Playing an On-chain Game

- What is this game all about?
- Commit scheme and hash function
- How to make game contract ready
- How to create staking address
- How to create Shelly era address
- What are the differences between address
- How to play it
- How to specify time for different deadlines
- What is `--invalid-before` and how it is different from `--invalid-hereafter`
- State management in eUTxO model
  - What was that state nft anyway and did we need it?
  - Could datum be used for state management? what is the limitation?
  - Transitions-as-transactions

## Redeemer, Datum and Contract Parameters

- Business rule and business logic
- UTxO model and account model
- What is role of datum, redeemer , contract parameter and script context
- The holy book of Plutus
- Data
- BuiltData
- How to define datum or redeemer
- How to construct parametrized datum or redeemer
- How to interact with them inside `repl`
- What `constr` actually is and why it is necessary
- What are the related functions such as `unstableMakeIsData`
- What those function do actually
- How to convert datum and redeemer for off-chain usage
- What should be inside to datum or redeemer
- CardanoScan

## Plutus Smart Contract Logic

- How to use datum, redeemer and contract parameters inside Plutus validator
- Definition section, condition Section, function section, compilation section
- Template haskell and `{-# INLINABLE #-}`
- Plutus functions
- Refactoring smart contract
- Hack the logic

## Deeper Look at Plutus

- Decoding Plutus
- Plutus Core
- Smart contract CBOR and binary
- Encoding and decoding addresses
