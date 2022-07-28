{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Ppbl.PpblValidator where

import              Ledger              hiding (singleton)
import              Ledger.Typed.Scripts
import              Ledger.Value        as Value
import              Ledger.Ada
import qualified    PlutusTx
import              PlutusTx.Prelude    hiding (Semigroup (..), unless)
import              Prelude             (Show (..))
import qualified    Prelude               as Haskell
import Plutus.V1.Ledger.Api (Data (B, Constr, I, List, Map), ToData, toData)


data PpblParameters = PpblParameters
    { ownerAddress :: !PubKeyHash
      ,ownerCut    :: !Integer

     }

PlutusTx.makeLift ''PpblParameters

data PpblDatum = PpblDatum
  {
    sellerAddress     :: !PubKeyHash
  , buyerAddress      :: !PubKeyHash
  , priceAmount       :: !Integer
  , cancelFees        :: !Integer
  
  }

PlutusTx.unstableMakeIsData ''PpblDatum

-- The Parameter allows you to control some aspects of the contract
-- The Datum can vary as long as it keeps the same definition types
-- The Redeemer is flexible and used for traffic control

data PpblRedeemer = Update | Buy | Cancel 
  deriving Show

PlutusTx.makeIsDataIndexed ''PpblRedeemer [('Update, 0), ('Buy, 1), ('Cancel, 2)]
PlutusTx.makeLift ''PpblRedeemer

{-# INLINABLE seekDatum #-}
seekDatum :: Maybe Datum -> Maybe PpblDatum
seekDatum md = do
    Datum d <- md
    PlutusTx.fromBuiltinData d

{-# INLINEABLE mkValidator #-}
mkValidator :: PpblParameters -> PpblDatum -> PpblRedeemer -> ScriptContext -> Bool
mkValidator pp dat red ctx =
  case red of

    Update     ->  traceIfFalse "Only Seller can Update Article Price"            signedBySeller &&
                   traceIfFalse "Seller Must Keep Contract Running"               createsContinuingSale
                    
    Buy    ->       traceIfFalse "Only Buyer can confirm Transaction"      signedByBuyer  &&
                    traceIfFalse "Price Paid to Seller"                    pricePaid &&
                    traceIfFalse "Cut Paid to Market Owner"                cutToOwner
                    
    
    Cancel     ->  traceIfFalse "Only Seller can Cancel Sale"             signedBySeller  &&
                    traceIfFalse "Cut Paid to Market Owner"               cutToOwner &&
                    traceIfFalse "Buyer if paid the Cancellation Fee"     feesToBuyer
                    


  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedBySeller :: Bool
    signedBySeller = txSignedBy info $ sellerAddress  dat

    signedByBuyer :: Bool
    signedByBuyer = txSignedBy info $ buyerAddress  dat

    signedByOwner :: Bool
    signedByOwner = txSignedBy info $ ownerAddress pp

    -- Update means that a UTXO must be left at contract address
    outputsToContract :: [TxOut]
    outputsToContract = getContinuingOutputs ctx

    createsContinuingSale :: Bool
    createsContinuingSale = length outputsToContract == 1

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one sale output"
    
    outputDatum :: PpblDatum
    outputDatum = case seekDatum $ txOutDatumHash ownOutput >>= flip findDatum info of
        Nothing -> traceError "Sale output datum not found"
        Just d  -> d

    -- Values Paid to Different Recipients

    valueToOwner :: Value
    valueToOwner = valuePaidTo info $ ownerAddress pp

    valueToSeller :: Value
    valueToSeller = valuePaidTo info $ sellerAddress dat

    valueToBuyer :: Value
    valueToBuyer = valuePaidTo info $ buyerAddress dat


    -- The value sent to Funder must be equal to what they deposited at the UTXO
    -- contributor must get tokenAmount bp of gimbals and lovelaceAmount bp...
    pricePaid :: Bool
    pricePaid = (getLovelace $ fromValue valueToSeller) >= (priceAmount dat)

    cutToOwner :: Bool
    cutToOwner =(getLovelace $ fromValue valueToOwner) >= (ownerCut pp)

-- We need to get the prospective buyer a fee for his committment
           
    feesToBuyer :: Bool
    feesToBuyer = (getLovelace $ fromValue valueToBuyer) >= (cancelFees dat)
    
    -- The content below is pretty much a matter of updating names------------------------------------------------------------------------------------

data PpblTypes

instance ValidatorTypes PpblTypes where
    type DatumType PpblTypes = PpblDatum
    type RedeemerType PpblTypes = PpblRedeemer

typedValidator :: PpblParameters -> TypedValidator PpblTypes
typedValidator bp =
  mkTypedValidator @PpblTypes
    ($$(PlutusTx.compile [||mkValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode bp)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = wrapValidator @PpblDatum @PpblRedeemer

validator :: PpblParameters -> Validator
validator = validatorScript . typedValidator