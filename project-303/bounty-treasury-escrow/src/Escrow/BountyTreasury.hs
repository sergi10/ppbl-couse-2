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

module Escrow.BountyTreasury where

import              Ledger              hiding (singleton)
import              Ledger.Typed.Scripts
import              Ledger.Value        as Value
import              Ledger.Ada
import qualified    PlutusTx
import              PlutusTx.Prelude    hiding (Semigroup (..), unless)

import              Escrow.BountyTypes

{-# INLINEABLE mkValidator #-}
mkValidator :: TreasuryParam -> WithdrawalDatum -> BountyDetails -> ScriptContext -> Bool
mkValidator tp dat b ctx =    traceIfFalse "Only Issuer can change Treasury"              signedByIssuer ||
                              traceIfFalse "Access token missing from input"              inputHasAuthToken &&
                              traceIfFalse "Access token missing from contract output"    contractOutputHasAuthToken &&
                              traceIfFalse "Output Value must match BountyDetails"        checkValueToBountyContract &&
                              traceIfFalse "Treasury must keep remaining lovelace"        treasuryGetsLovelaceBack &&
                              traceIfFalse "Treasury must keep remaining tokens"          treasuryGetsTokensBack &&
                              traceIfFalse "redeemer is not datum"                        checkReIsOutDat

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByIssuer :: Bool
    signedByIssuer = txSignedBy info $ tTreasuryIssuerPkh tp

    -- Create a list of all CurrencySymbol in tx input
    inVals :: [CurrencySymbol]
    inVals = symbols $ valueSpent info

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
    checkValueToBountyContract :: Bool
    checkValueToBountyContract =  getLovelace (fromValue toBountyContract) >= lovelaceAmount b &&
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

    -- Compare Values from and to Treasury to make sure that Treasury gets the right value back.
    treasuryGetsLovelaceBack :: Bool
    treasuryGetsLovelaceBack = getLovelace ( fromValue treasuryInputValue) - getLovelace ( fromValue treasuryOutputValue) <= getLovelace ( fromValue toBountyContract)

    treasuryGetsTokensBack :: Bool
    treasuryGetsTokensBack = valueOf treasuryInputValue (tBountyTokenPolicyId tp) (tBountyTokenName tp) - valueOf treasuryOutputValue (tBountyTokenPolicyId tp) (tBountyTokenName tp) <= valueOf toBountyContract (tBountyTokenPolicyId tp) (tBountyTokenName tp)

    getEscrowDatum :: Maybe BountyEscrowDatum
    getEscrowDatum = let os = [ o | o <- txInfoOutputs info, txOutValue o == toBountyContract ] in
                  case os of
                    [o] -> escrowDatum o (`findDatum` info)
                    _   -> Nothing

    checkReIsOutDat :: Bool
    checkReIsOutDat = case getEscrowDatum of
      Nothing -> False
      Just ns -> bedIssuerPkh   ns == issuerPkh b &&
                 bedContributorPkh ns == contributorPkh b &&
                 bedLovelaceAmount ns == lovelaceAmount b &&
                 bedTokenAmount ns == tokenAmount b &&
                 bedExpirationTime ns == expirationTime b

typedValidator :: TreasuryParam -> TypedValidator TreasuryTypes
typedValidator tp =
  mkTypedValidator @TreasuryTypes
    ($$(PlutusTx.compile [||mkValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode tp)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = wrapValidator @WithdrawalDatum @BountyDetails

validator :: TreasuryParam -> Validator
validator = validatorScript . typedValidator
