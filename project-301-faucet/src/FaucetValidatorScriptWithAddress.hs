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


module FaucetValidatorScriptWithAddress
  ( FaucetParams (..)
  , validator
  ) where


import              Data.Aeson                (ToJSON, FromJSON)
import              GHC.Generics              (Generic)
import              Schema                    (ToSchema)
import              Ledger              hiding (singleton)
import              Ledger.Address
import              Ledger.Typed.Scripts
import              Ledger.Value        as Value
import              Ledger.Ada
import qualified    PlutusTx
import              PlutusTx.Prelude    hiding (Semigroup (..), unless)
import              Prelude             (Show (..))
import qualified    Prelude                   as Pr

-- Simple Faucet validator script

-- Usage:
-- Expect one utxo at each contract.
-- Take that utxo as input
-- Create a new utxo with the "change" as output

-- This contract provides an example of using Validator Parameters.

-- For now, the Datum and Redeemer are not used in contract logic
-- Transactions will still have to match the type Integer for Datum and Redeemer
-- The context matters: we want to see that the PPBLSummer2022 token is in the transaction inputs and outputs.

data FaucetParams = FaucetParams
  { accessTokenSymbol   :: !CurrencySymbol
  , accessTokenName     :: !TokenName
  , faucetTokenSymbol   :: !CurrencySymbol
  , faucetTokenName     :: !TokenName
  , withdrawalAmount    :: !Integer
  } deriving (Pr.Eq, Pr.Ord, Show, Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''FaucetParams

{-# INLINEABLE faucetValidator #-}
faucetValidator :: FaucetParams -> Integer -> Address -> ScriptContext -> Bool
faucetValidator faucet _ withdrawalAddress ctx =  traceIfFalse "Input needs PPBLSummer2022 token"           inputHasAccessToken &&
                                                  traceIfFalse "PPBLSummer2022 token must return to sender" outputHasAccessToken &&
                                                  traceIfFalse "Faucet token must be distributed to sender" outputHasFaucetToken &&
                                                  traceIfFalse "Must return remaining tokens to contract"   faucetContractGetsRemainingTokens &&
                                                  traceIfFalse "Do we need to check datum"                  checkDatumIsOk
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    faucetOutputs :: [TxOut]
    faucetOutputs = txInfoOutputs info

    -- receiverPkh :: PubKeyHash
    -- receiverPkh = head $ txInfoSignatories info

    addressGetsFaucetTokens :: [TxOut] -> Address -> Bool
    addressGetsFaucetTokens [] _ = False
    addressGetsFaucetTokens (x:xs) addr
      | checkAddress && checkValue  = True
      | otherwise                   = addressGetsFaucetTokens xs addr
      where
        checkAddress = txOutAddress x == addr
        checkValue = valueOf (txOutValue x) (accessTokenSymbol faucet) (accessTokenName faucet) >= (withdrawalAmount faucet)

    allTokens :: [CurrencySymbol]
    allTokens = symbols $ valueSpent info

    inputHasAccessToken :: Bool
    inputHasAccessToken = (accessTokenSymbol faucet) `elem` allTokens

    -- valueToReceiver :: Value
    -- valueToReceiver = valuePaidTo info receiverPkh

    outputHasAccessToken :: Bool
    outputHasAccessToken = True
    -- outputHasAccessToken = (valueOf valueToReceiver (accessTokenSymbol faucet) (accessTokenName faucet)) >= 1

    outputHasFaucetToken :: Bool
    outputHasFaucetToken = addressGetsFaucetTokens faucetOutputs withdrawalAddress

    -- The UTXO input from Faucet
    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "faucet input missing"
        Just i  -> txInInfoResolved i

    -- The UTXO output back to Faucet
    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o -- There must be exactly ONE output UTXO
        _   -> traceError "expected exactly one faucet output"

    faucetInputValue :: Value
    faucetInputValue = txOutValue ownInput

    faucetOutputValue :: Value
    faucetOutputValue = txOutValue ownOutput

    faucetContractGetsRemainingTokens :: Bool
    faucetContractGetsRemainingTokens = (valueOf faucetInputValue (faucetTokenSymbol faucet) (faucetTokenName faucet)) - (withdrawalAmount faucet) <= (valueOf faucetOutputValue (faucetTokenSymbol faucet) (faucetTokenName faucet))

    checkDatumIsOk :: Bool
    checkDatumIsOk = True

data FaucetTypes

instance ValidatorTypes FaucetTypes where
    type DatumType FaucetTypes = Integer
    type RedeemerType FaucetTypes = Address

typedValidator :: FaucetParams -> TypedValidator FaucetTypes
typedValidator faucet =
  mkTypedValidator @FaucetTypes
    ($$(PlutusTx.compile [||faucetValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode faucet)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = wrapValidator @Integer @Address


validator :: FaucetParams -> Validator
validator = validatorScript . typedValidator