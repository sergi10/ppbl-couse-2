{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TreasuryAndEscrow.TreasuryValidator where

import              Ledger              hiding (singleton)
import              Ledger.Typed.Scripts
import              Ledger.Value        as Value
import              Ledger.Ada
import qualified    PlutusTx
import              PlutusTx.Prelude    hiding (Semigroup (..), unless)

import              TreasuryAndEscrow.Types

-- Updated 2022-10-25
-- Note: this validator does not check that the Bounty Details specified with TreasuryAction matches the Datum on Escrow

{-# INLINEABLE mkValidator #-}
mkValidator :: TreasuryParam -> TreasuryDatum -> TreasuryAction -> ScriptContext -> Bool
mkValidator tp dat action ctx =
  case action of
    (Commit b)  ->  traceIfFalse "Access token missing from input"              inputHasAuthToken &&
                    traceIfFalse "Access token missing from contract output"    contractOutputHasAuthToken &&
                    traceIfFalse "Output Value must match BountyDetails"        (checkValueToBountyContract b) &&
                    traceIfFalse "Treasury must keep remaining lovelace"        (treasuryGetsLovelaceBack b) &&
                    traceIfFalse "Treasury must keep remaining tokens"          (treasuryGetsTokensBack b)
    Manage      ->  traceIfFalse "Only Issuer can manage Treasury"              inputHasIssuerToken

  where
    -- used in all cases:
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- Create a list of all CurrencySymbol in tx input
    inVals :: [CurrencySymbol]
    inVals = symbols $ valueSpent info

    -- case action -> Manage:
    -- If the TreasuryAction is "Manage", then input must have an IssuerToken
    inputHasIssuerToken :: Bool
    inputHasIssuerToken = tIssuerTokenPolicyId tp `elem` inVals

    -- case action -> Commit:
    -- Check that list of CurrencySymbols includes Auth CurrencySymbol
    inputHasAuthToken :: Bool
    inputHasAuthToken = tAccessTokenPolicyId tp `elem` inVals

    -- The Value to be included in Bounty Contract UTXO
    toBountyContract :: Value
    toBountyContract = valueLockedBy info (bountyContractHash tp)

    -- Check that the Auth Token is sent to Bounty Contract UTXO
    contractOutputHasAuthToken :: Bool
    contractOutputHasAuthToken = tAccessTokenPolicyId tp `elem` symbols toBountyContract

    -- Check that the Value sent to Contract UTXO matches what is specified in the Redeemer
    -- Note: For now, we can just remember to match Treasury Redeemer to Bounty Datum
    -- when we build transactions
    checkValueToBountyContract :: BountyDetails -> Bool
    checkValueToBountyContract b = getLovelace (fromValue toBountyContract) >= lovelaceAmount b &&
                                   valueOf toBountyContract (tBountyTokenPolicyId tp) (tBountyTokenName tp) >= tokenAmount b

    -- The UTXO input from Treasury
    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "treasury input missing"
        Just i  -> txInInfoResolved i

    -- The UTXO output back to Treasury
    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o -- There must be exactly ONE output UTXO
        _   -> traceError "expected exactly one treasury output"

    -- Values of each
    treasuryInputValue :: Value
    treasuryInputValue = txOutValue ownInput

    treasuryOutputValue :: Value
    treasuryOutputValue = txOutValue ownOutput

    -- Compare Values from and to Treasury to make sure that Treasury gets the right value back
    treasuryGetsLovelaceBack :: BountyDetails -> Bool
    treasuryGetsLovelaceBack b = lovelaceToTreasury == lovelaceFromTreasury - lovelaceToBounty
        where
            lovelaceFromTreasury = getLovelace (fromValue treasuryInputValue)
            lovelaceToTreasury = getLovelace (fromValue treasuryOutputValue)
            lovelaceToBounty = lovelaceAmount b

    treasuryGetsTokensBack :: BountyDetails -> Bool
    treasuryGetsTokensBack b = gimbalsToTreasury == gimbalsFromTreasury - gimbalsToBounty
        where
            gimbalsFromTreasury = valueOf treasuryInputValue (tBountyTokenPolicyId tp) (tBountyTokenName tp)
            gimbalsToTreasury = valueOf treasuryOutputValue (tBountyTokenPolicyId tp) (tBountyTokenName tp)
            gimbalsToBounty = tokenAmount b

typedValidator :: TreasuryParam -> TypedValidator TreasuryTypes
typedValidator tp =
  mkTypedValidator @TreasuryTypes
    ($$(PlutusTx.compile [||mkValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode tp)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = wrapValidator @TreasuryDatum @TreasuryAction

validator :: TreasuryParam -> Validator
validator = validatorScript . typedValidator
