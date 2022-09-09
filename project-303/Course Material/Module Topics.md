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

- The holy book of Plutus
- BuiltData
- Data
- What is role of datum
- What is role of redeemer
- What is role of contract parameters
- How to define datum or redeemer
- How to construct parametrized datum or redeemer
- How to interact with them inside `repl`
- What `constr` actually is and why it is necessary
- What are the related functions such as `unstableMakeIsData`
- What those function do actually
- What should be inside to datum or redeemer
- How to convert datum and redeemer for off-chain usage
- How to use datum, redeemer and contract parameters inside Plutus validator
