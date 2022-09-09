# Project 301 - Mastery Level 4: Register Your Faucet with Tx Metadata
## Register Your PPBL Faucet with Metadata

## Before You Start:
- Complete Steps 1-4 of Project 301
- Complete Mastery Levels 1-3 of Project 301
- Confirm that your Faucet Contract is compiled for *Pre-Production* Testnet (for example, by making sure that the Access Token and Faucet Token Policy IDs represent Native Assets on Pre-Production)
- Generate an address on Pre-Production for the Contract that you compiled in Step 2, and that you added to the Front End repo in Step 4.
- Check here to see that one of the addresses in the `faucetList` matches yours: https://gitlab.com/gimbalabs/plutus-pbl-summer-2022/ppbl-front-end-template/-/blob/main/cardano/plutus/ppbl-preprod-faucet-list.js
- Lock tokens at your Contract Address [(Review docs here)](https://gitlab.com/gimbalabs/plutus-pbl-summer-2022/ppbl-course-02/-/tree/master/project-301-faucet)
- Make sure to note the integer that was used as Datum in your locking transaction. You'll need it here.

## What to do: Build a Transaction with Metadata
### Prepare a Metadata File:
Create a file called `faucet-reg-metadata.json`, with the following contents:
```
{
	"1618033988": {
		"contractAddress": "<ADDRESS OF YOUR FAUCET CONTRACT>",
		"policyId": "<POLICY ID OF THE TOKEN DISTRIBUTED BY YOUR FAUCET>",
		"tokenName": "<NAME OF YOUR TOKEN (PLAIN TEXT)>",
		"datumInt": "<INTEGER USED AS DATUM IN YOUR CONTRACT>",
		"withdrawalAmount": "<NUMBER OF TOKENS THAT CAN BE WITHDRAWN>"
	}
}
```
- Note - all values are strings, be sure to include quotation marks for each field.

### Submit a Transaction with Metadata

To quickly add metadata to the blockchain, you can send a transaction to yourself. For example, note that are no `--tx-out`'s in this `transaction build`:

```
cardano-cli transaction build \
--alonzo-era \
--testnet-magic 1 \
--tx-in $TXIN \
--change-address $SENDER \
--metadata-json-file faucet-reg-metadata.json \
--out-file faucet-registration.raw

cardano-cli transaction sign \
--signing-key-file $SENDERKEY \
--testnet-magic 1 \
--tx-body-file faucet-registration.raw \
--out-file faucet-registration.signed

cardano-cli transaction submit \
--tx-file faucet-registration.signed \
--testnet-magic 1
```

## You will know you are successful if:
- You can run the latest Front End Template locally (make sure to `git pull` the latest changes) and navigate to https://localhost:3000/registered-faucets.
- Do you see your token on this page?
- Try to unlock tokens from your Faucet. If it works, congratulations, you completed this project!

## How it works
Study the following files:
- https://gitlab.com/gimbalabs/plutus-pbl-summer-2022/ppbl-front-end-template/-/blob/main/pages/registered-faucets.tsx
- https://gitlab.com/gimbalabs/plutus-pbl-summer-2022/ppbl-front-end-template/-/blob/main/components/faucets/FaucetUnlockingComponentWithMetadata.tsx

What to look for:
1. Look at how GraphQL Queries are used in each file.
2. Look at how that data is consumed and used in components that appear on the page
3. Look at how Metadata is used to populate the `FaucetMetadata` object whose type is defined in `/cardano/Types.ts`

## Following Up:
1. How might you extend upon this mini-project?
2. Bring your questions to Live Coding on Wednesdays and Thursdays at 1430 UTC
