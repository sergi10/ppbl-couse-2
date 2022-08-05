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


module FaucetValidatorScript where

import              Ledger              hiding (singleton)
import              Ledger.Typed.Scripts
import              Ledger.Value        as Value
import              Ledger.Ada
import qualified    PlutusTx
import              PlutusTx.Prelude    hiding (Semigroup (..), unless)
import              Prelude             (Show (..))
import Ledger.Address

-- We're going to need some parameters - great chance to apply this concept!

-- The Redeemer could be irrelevant.
-- The Datum could be used just to show how it works.
-- The context matters: we want to see that the PPBLSummer2022 token is in the transaction inputs and outputs.

{-# INLINEABLE faucetValidator #-}
faucetValidator :: Integer -> Integer -> ScriptContext -> Bool
faucetValidator _ _ ctx =   traceIfFalse "Input needs PPBLSummer2022 token" inputHasToken &&
                            traceIfFalse "PPBLSummer2022 token must return to sender" outputHasToken &&
                            traceIfFalse "Withdrawal limit exceeded" checkWithdrawalLimit &&
                            traceIfFalse "Do we need to check datum" checkDatumIsOk
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    inputHasToken :: Bool
    inputHasToken = True

    outputHasToken :: Bool
    outputHasToken = True

    checkWithdrawalLimit :: Bool
    checkWithdrawalLimit = True

    checkDatumIsOk :: Bool
    checkDatumIsOk = True

data FaucetTypes

instance ValidatorTypes FaucetTypes where
    type DatumType FaucetTypes = Integer
    type RedeemerType FaucetTypes = Integer

typedValidator :: TypedValidator FaucetTypes
typedValidator =
  mkTypedValidator @FaucetTypes
    $$(PlutusTx.compile [||faucetValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = wrapValidator @Integer @Integer


validator :: Validator
validator = validatorScript typedValidator