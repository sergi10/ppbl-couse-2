{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}

module Escrow.BountyEscrow where

import              Ledger              hiding (singleton)
import              Ledger.Typed.Scripts
import              Ledger.Value        as Value
import              Ledger.Ada
import qualified    PlutusTx
import              PlutusTx.Prelude    hiding (Semigroup (..), unless)

import              Escrow.BountyTypes

{-# INLINEABLE mkValidator #-}
mkValidator :: BountyParam -> BountyEscrowDatum -> BountyAction -> ScriptContext -> Bool
mkValidator bp dat action ctx =
  case action of
    Cancel      ->  traceIfFalse "Only Issuer can Cancel Bounty"                signedByIssuer &&
                    traceIfFalse "Can only cancel bounty after deadline"        deadlineReached
    Update      ->  traceIfFalse "Only Issuer can Update Bounty"                signedByIssuer &&
                    traceIfFalse "Update must create one new Bounty UTXO"       createsContinuingBounty &&
                    traceIfFalse "Output UTXO value must be geq datum specs"    outputFulfillsValue &&
                    traceIfFalse "Updated datum is wrong"                       checkDatum
    Distribute  ->  traceIfFalse "Issuer must sign to distribute bounty"        signedByIssuer &&
                    traceIfFalse "Contributor must receive full bounty values"  outputFulfillsBounty

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    bCursym :: CurrencySymbol
    bCursym = bountyTokenPolicyId bp

    bTokenN :: TokenName
    bTokenN = bountyTokenName bp

    signedByIssuer :: Bool
    signedByIssuer = txSignedBy info $ bedIssuerPkh dat

    deadlineReached :: Bool
    deadlineReached = contains (from $ bedExpirationTime dat) $ txInfoValidRange info

    -- Update means that a UTXO must be left at contract address
    outputsToContract :: [TxOut]
    outputsToContract = getContinuingOutputs ctx

    createsContinuingBounty :: Bool
    createsContinuingBounty = length outputsToContract == 1

    outputContainsValue :: [TxOut] -> Bool
    outputContainsValue [x]   = valueOf (txOutValue x) bCursym bTokenN >= bedTokenAmount dat &&
                                getLovelace (fromValue $ txOutValue x) >= bedLovelaceAmount dat
    outputContainsValue _     = False

    outputFulfillsValue :: Bool
    outputFulfillsValue = outputContainsValue outputsToContract

    valueToContributor :: Value
    valueToContributor = valuePaidTo info $ bedContributorPkh dat

    -- The value sent to Contributor must be at least the amount specified by bounty
    -- contributor must get tokenAmount bp of gimbals and lovelaceAmount bp...
    outputFulfillsBounty :: Bool
    outputFulfillsBounty = valueOf valueToContributor bCursym bTokenN >= bedTokenAmount dat &&
                           getLovelace ( fromValue valueToContributor) >= bedLovelaceAmount dat

    ownInputVal :: Value
    ownInputVal = case findOwnInput ctx of
                Just iv -> txOutValue $ txInInfoResolved iv
                Nothing -> error ()

    getEscrowDatum :: Maybe BountyEscrowDatum
    getEscrowDatum = let os = [ o | o <- txInfoOutputs info, txOutValue o == ownInputVal ] in
                  case os of
                    -- escrowDatum defined in BountyTypes.hs to use in escrow and treasury
                    [o] -> escrowDatum o (`findDatum` info)
                    _   -> Nothing

    checkDatum :: Bool
    checkDatum = case getEscrowDatum of
      Nothing -> False
      Just ns -> bedIssuerPkh      ns == bedIssuerPkh dat      &&
                 bedContributorPkh ns == bedContributorPkh dat &&
                 bedLovelaceAmount ns >= bedLovelaceAmount dat &&
                 bedTokenAmount    ns >= bedTokenAmount dat    &&
                 bedExpirationTime ns >= bedExpirationTime dat

typedValidator :: BountyParam -> TypedValidator EscrowTypes
typedValidator bp =
  mkTypedValidator @EscrowTypes
    ($$(PlutusTx.compile [||mkValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode bp)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = wrapValidator @BountyEscrowDatum @BountyAction

validator :: BountyParam -> Validator
validator = validatorScript . typedValidator
