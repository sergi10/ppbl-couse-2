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


module FaucetValidatorScriptMinimumLogic
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

-- Even Simpler Faucet Validator -> For Testing Front End Transactions

data FaucetParams = FaucetParams
  { accessTokenSymbol   :: !CurrencySymbol
  , accessTokenName     :: !TokenName
  , faucetTokenSymbol   :: !CurrencySymbol
  , faucetTokenName     :: !TokenName
  , withdrawalAmount    :: !Integer
  } deriving (Pr.Eq, Pr.Ord, Show, Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''FaucetParams

-- Maybe add a datum check?

{-# INLINEABLE faucetValidator #-}
faucetValidator :: FaucetParams -> Integer -> Integer -> ScriptContext -> Bool
faucetValidator faucet _ _ ctx =  traceIfFalse "Input needs PPBLSummer2022 token"           inputHasAccessToken &&
                                  traceIfFalse "Must return remaining tokens to contract"   faucetContractGetsRemainingTokens

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    allTokens :: [CurrencySymbol]
    allTokens = symbols $ valueSpent info

    inputHasAccessToken :: Bool
    inputHasAccessToken = (accessTokenSymbol faucet) `elem` allTokens

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

    expectedTokensReturnedToFaucet :: Integer
    expectedTokensReturnedToFaucet = (valueOf faucetInputValue (faucetTokenSymbol faucet) (faucetTokenName faucet)) - (withdrawalAmount faucet)

    tokensReturnedToFaucet :: Integer
    tokensReturnedToFaucet = (valueOf faucetOutputValue (faucetTokenSymbol faucet) (faucetTokenName faucet))

    faucetContractGetsRemainingTokens :: Bool
    faucetContractGetsRemainingTokens = tokensReturnedToFaucet >= expectedTokensReturnedToFaucet


data FaucetTypes

instance ValidatorTypes FaucetTypes where
    type DatumType FaucetTypes = Integer
    type RedeemerType FaucetTypes = Integer

typedValidator :: FaucetParams -> TypedValidator FaucetTypes
typedValidator faucet =
  mkTypedValidator @FaucetTypes
    ($$(PlutusTx.compile [||faucetValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode faucet)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = wrapValidator @Integer @Integer


validator :: FaucetParams -> Validator
validator = validatorScript . typedValidator