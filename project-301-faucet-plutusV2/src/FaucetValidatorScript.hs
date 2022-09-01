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


module FaucetValidatorScript
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

data FaucetParams = FaucetParams
  { accessTokenSymbol   :: !CurrencySymbol
  , accessTokenName     :: !TokenName
  , faucetTokenSymbol   :: !CurrencySymbol
  , faucetTokenName     :: !TokenName
  , withdrawalAmount    :: !Integer
  } deriving (Pr.Eq, Pr.Ord, Show, Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''FaucetParams

newtype FaucetRedeemer = FaucetRedeemer {senderPkh :: PubKeyHash}

PlutusTx.unstableMakeIsData ''FaucetRedeemer
PlutusTx.makeLift ''FaucetRedeemer

{-# INLINEABLE faucetValidator #-}
faucetValidator :: FaucetParams -> Integer -> FaucetRedeemer -> ScriptContext -> Bool
faucetValidator faucet _ receiver ctx = traceIfFalse "Input needs PPBLSummer2022 token"           inputHasAccessToken &&
                                        traceIfFalse "PPBLSummer2022 token must return to sender" outputHasAccessToken &&
                                        traceIfFalse "Faucet token must be distributed to sender" outputHasFaucetToken &&
                                        traceIfFalse "Must return remaining tokens to contract"   faucetContractGetsRemainingTokens
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    receiverPkh :: PubKeyHash
    receiverPkh = senderPkh receiver

    allTokens :: [CurrencySymbol]
    allTokens = symbols $ valueSpent info

    inputHasAccessToken :: Bool
    inputHasAccessToken = (accessTokenSymbol faucet) `elem` allTokens

    valueToReceiver :: Value
    valueToReceiver = valuePaidTo info receiverPkh

    outputHasAccessToken :: Bool
    outputHasAccessToken = (valueOf valueToReceiver (accessTokenSymbol faucet) (accessTokenName faucet)) >= 1

    tokensToWithdraw :: Integer
    tokensToWithdraw = (valueOf valueToReceiver (faucetTokenSymbol faucet) (faucetTokenName faucet))

    outputHasFaucetToken :: Bool
    outputHasFaucetToken = tokensToWithdraw >= (withdrawalAmount faucet)

    -- The UTXO input from Faucet
    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "faucet input missing"
        Just i  -> txInInfoResolved i

    -- The UTXO output back to Faucet
    ownOutput :: Maybe TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> Just o -- There must be exactly ONE output UTXO
        _   -> Nothing

    faucetInputValue :: Value
    faucetInputValue = txOutValue ownInput

    faucetOutputValue :: Value
    faucetOutputValue = txOutValue ownOutput

    expectedTokensToFaucet :: Integer
    expectedTokensToFaucet = (valueOf faucetInputValue (faucetTokenSymbol faucet) (faucetTokenName faucet)) - (withdrawalAmount faucet)

    actualTokensToFaucet :: Integer
    actualTokensToFaucet = (valueOf faucetOutputValue (faucetTokenSymbol faucet) (faucetTokenName faucet))

    faucetContractGetsRemainingTokens :: Bool
    faucetContractGetsRemainingTokens = case expectedTokensToFaucet of
      0 -> tokensToWithdraw == (withdrawalAmount faucet)
      _ -> expectedTokensToFaucet <= actualTokensToFaucet


data FaucetTypes

instance ValidatorTypes FaucetTypes where
    type DatumType FaucetTypes = Integer
    type RedeemerType FaucetTypes = FaucetRedeemer

typedValidator :: FaucetParams -> TypedValidator FaucetTypes
typedValidator faucet =
  mkTypedValidator @FaucetTypes
    ($$(PlutusTx.compile [||faucetValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode faucet)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = wrapValidator @Integer @FaucetRedeemer


validator :: FaucetParams -> Validator
validator = validatorScript . typedValidator