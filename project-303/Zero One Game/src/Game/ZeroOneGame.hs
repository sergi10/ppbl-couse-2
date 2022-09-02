{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Game.ZeroOneGame where

import           Ledger               hiding (singleton)
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Ledger.Value
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)

import           Game.GameTypes

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE seek #-}
seek :: Maybe Datum -> Maybe GameDatum
seek md = do
    Datum d <- md
    PlutusTx.fromBuiltinData d

{-# INLINABLE mkGameValidator #-}
mkGameValidator :: GameParams -> BuiltinByteString -> BuiltinByteString -> GameDatum -> GameRedeemer -> ScriptContext -> Bool
mkGameValidator gp bsZero' bsOne' dat red ctx =
    traceIfFalse "State NFT Missing From Input" (assetClassValueOf (txOutValue ownInput) (stateNFT gp) == 1) &&

    case (dat, red) of

        (GameDatum bs Nothing, Play choice) ->
            traceIfFalse "Not Signed By Second Player"          (txSignedBy info (unPaymentPubKeyHash $ secondPlayerPaymentPKH gp)) &&
            traceIfFalse "First Player's Bet UTxO is Wrong"     (lovelaces (txOutValue ownInput) == gBet gp)                        &&
            traceIfFalse "Second Player's Bet Amount is Wrong"  (lovelaces (txOutValue ownOutput) == (2 * gBet gp))                 &&
            traceIfFalse "Wrong Output Datum"                   (outputDatum == GameDatum bs (Just choice))                         &&
            traceIfFalse "Game Deadline Has Been Reached"       (to (gPlayDeadline gp) `contains` txInfoValidRange info)            &&
            traceIfFalse "State NFT Missing From Output"        (assetClassValueOf (txOutValue ownOutput) (stateNFT gp) == 1)

        (GameDatum bs (Just choice), Reveal nonce) ->
            traceIfFalse "Not Signed By First Player"           (txSignedBy info (unPaymentPubKeyHash $ firstPlayerPaymentPKH gp))  &&
            traceIfFalse "Commit Mismatch"                      (checkNonce bs nonce choice)                                        &&
            traceIfFalse "Reveal Deadline Has Been Reached"     (to (gRevealDeadline gp) `contains` txInfoValidRange info)          &&
            traceIfFalse "Bet Amount Must Be Doubled or More"   (lovelaces (txOutValue ownInput) >= (2 * gBet gp))                  &&
            traceIfFalse "State NFT Must Go To First Player"    nftToFirst

        (GameDatum _ Nothing, ClaimByFirstPlayer) ->
            traceIfFalse "Not Signed By First Player"           (txSignedBy info (unPaymentPubKeyHash $ firstPlayerPaymentPKH gp))  &&
            traceIfFalse "Game Deadline Has NOT Been Reached"   (from (1 + gPlayDeadline gp) `contains` txInfoValidRange info)      &&
            traceIfFalse "First Player's Bet UTxO is Wrong"     (lovelaces (txOutValue ownInput) == gBet gp)                        &&
            traceIfFalse "State NFT Must Go to First Player"    nftToFirst

        (GameDatum _ (Just _), ClaimBySecondPlayer) ->
            traceIfFalse "Not Signed By Second Player"          (txSignedBy info (unPaymentPubKeyHash $ secondPlayerPaymentPKH gp)) &&
            traceIfFalse "Reveal Deadline Has NOT Been Reached" (from (1 + gRevealDeadline gp) `contains` txInfoValidRange info)    &&
            traceIfFalse "Bet Amount Must Be Doubled or More"   (lovelaces (txOutValue ownInput) >= (2 * gBet gp))                  &&
            traceIfFalse "State NFT must go to first player"    nftToFirst

        _ -> False

  where

    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "Game Input Missing"
        Just i  -> txInInfoResolved i

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "Expected Exactly One Game Output"

    outputDatum :: GameDatum
    outputDatum = case seek $ txOutDatumHash ownOutput >>= flip findDatum info of
        Nothing -> traceError "Game Output Datum Not Found"
        Just d  -> d

    checkNonce :: BuiltinByteString -> BuiltinByteString -> PlayerChoice -> Bool
    checkNonce bs nonce secondPC = sha2_256 (nonce `appendByteString` firstPC) == bs
      where
        firstPC :: BuiltinByteString
        firstPC = case secondPC of
            Zero -> bsZero'
            One  -> bsOne'

    nftToFirst :: Bool
    nftToFirst = assetClassValueOf (valuePaidTo info $ unPaymentPubKeyHash $ firstPlayerPaymentPKH gp) (stateNFT gp) == 1

bsZero, bsOne :: BuiltinByteString
bsZero = "0"
bsOne  = "1"

typedGameValidator :: GameParams -> Scripts.TypedValidator GameTypes
typedGameValidator game = Scripts.mkTypedValidator @GameTypes
    ($$(PlutusTx.compile [|| mkGameValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode game
        `PlutusTx.applyCode` PlutusTx.liftCode bsZero
        `PlutusTx.applyCode` PlutusTx.liftCode bsOne)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @GameDatum @GameRedeemer

gameValidator :: GameParams -> Validator
gameValidator = Scripts.validatorScript . typedGameValidator
