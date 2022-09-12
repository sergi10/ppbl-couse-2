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
import           PlutusTx.Prelude     hiding (Semigroup(..), unless, maybe)

import           Game.GameTypes

bsZero, bsOne :: BuiltinByteString
bsZero = "0"
bsOne  = "1"

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE deserializeDatum #-}
deserializeDatum :: Maybe Datum -> Maybe GameDatum
deserializeDatum md = do
    Datum d <- md
    PlutusTx.fromBuiltinData d

{-# INLINABLE firstPlayerAdaForNFT #-}
firstPlayerAdaForNFT :: Integer
firstPlayerAdaForNFT = Ada.getLovelace minAdaTxOut

-- mkGameValidator :: BuiltinData -> BuiltinByteString -> BuiltinByteString -> BuiltinData -> BuiltinData -> BuiltinData -> ()

{-# INLINABLE mkGameValidator #-}
mkGameValidator :: GameParams -> BuiltinByteString -> BuiltinByteString -> GameDatum -> GameRedeemer -> ScriptContext -> Bool
mkGameValidator gp bsZero' bsOne' dat red ctx =
    traceIfFalse "State NFT Missing From Input" (assetClassValueOf (txOutValue ownInput) (stateNFT gp) == 1) &&

    case (dat, red) of

        (GameDatum bs Nothing, Play choice) ->
            traceIfFalse "Not Signed By Second Player"          (txSignedBy info secondPlayerPaymentPKH)                                    &&
            traceIfFalse "First Player's Bet UTxO is Wrong"     (lovelaces (txOutValue ownInput) == gBet gp + firstPlayerAdaForNFT)         &&
            traceIfFalse "Second Player's Bet Amount is Wrong"  (lovelaces (txOutValue ownOutput) == (2 * gBet gp + firstPlayerAdaForNFT))  &&
            traceIfFalse "Wrong Output Datum"                   (outputDatum == GameDatum bs (Just choice))                                 &&
            traceIfFalse "Game Deadline Has Been Reached"       ((to (gPlayDeadline gp)) `contains` (txInfoValidRange info))                &&
            traceIfFalse "State NFT Missing From Output"        (assetClassValueOf (txOutValue ownOutput) (stateNFT gp) == 1)

        (GameDatum bs (Just choice), Reveal nonce) ->
            traceIfFalse "Not Signed By First Player"           (txSignedBy info firstPlayerPaymentPKH)                                     &&
            traceIfFalse "Mismatch Choice, Second Player Won"   (checkNonce bs nonce choice)                                                &&
            traceIfFalse "Reveal Deadline Has Been Reached"     ((to (1 + gRevealDeadline gp)) `contains` (txInfoValidRange info))          &&
            traceIfFalse "Second Player's Bet UTxO is Wrong"    (lovelaces (txOutValue ownInput) == (2 * gBet gp + firstPlayerAdaForNFT))   &&
            traceIfFalse "State NFT Must Go To First Player"    nftToFirstPlayer

        (GameDatum _ Nothing, ClaimByFirstPlayer) ->
            traceIfFalse "Not Signed By First Player"           (txSignedBy info firstPlayerPaymentPKH)                                     &&
            traceIfFalse "Game Deadline Has NOT Been Reached"   ((from (gPlayDeadline gp)) `contains` (txInfoValidRange info))              &&
            traceIfFalse "First Player's Bet UTxO is Wrong"     ((lovelaces (txOutValue ownInput)) == (gBet gp + firstPlayerAdaForNFT))     &&
            traceIfFalse "State NFT Must Go to First Player"    nftToFirstPlayer

        (GameDatum _ (Just _), ClaimBySecondPlayer) ->
            traceIfFalse "Not Signed By Second Player"          (txSignedBy info secondPlayerPaymentPKH)                                    &&
            traceIfFalse "Reveal Deadline Has NOT Been Reached" ((from (1 + gRevealDeadline gp)) `contains` (txInfoValidRange info))        &&
            traceIfFalse "Second Player's Bet UTxO is Wrong"    ((lovelaces (txOutValue ownInput)) == (2 * gBet gp + firstPlayerAdaForNFT)) &&
            traceIfFalse "First Player Address Mismatch"        (checkOutputAddress $ firstPlayerAddress gp)                                &&
            traceIfFalse "State NFT must go to first player"    (nftToFirstPlayer  &&  adaToFirstPlayer)

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
    outputDatum = case deserializeDatum $ txOutDatumHash ownOutput >>= flip findDatum info of
        Nothing -> traceError "Game Output Datum Not Found"
        Just d  -> d

    checkNonce :: BuiltinByteString -> BuiltinByteString -> PlayerChoice -> Bool
    checkNonce bs nonce secondPC = sha2_256 (nonce `appendByteString` firstPC) == bs
      where
        firstPC :: BuiltinByteString
        firstPC = case secondPC of
            Zero -> bsZero'
            One  -> bsOne'

    firstPlayerPaymentPKH :: PubKeyHash
    firstPlayerPaymentPKH = case toPubKeyHash $ firstPlayerAddress gp of
        Just pkh -> pkh
        Nothing -> traceError "No Payment Public Key Hash"

    secondPlayerPaymentPKH :: PubKeyHash
    secondPlayerPaymentPKH = case toPubKeyHash $ secondPlayerAddress gp of
        Just pkh -> pkh
        Nothing -> traceError "No Payment Public Key Hash"

    allTxOutputs :: [TxOut]
    allTxOutputs = txInfoOutputs info

    outputAddress :: Address -> TxOut
    outputAddress addr = case [ o | o <- allTxOutputs , txOutAddress o == addr ] of
        [o] -> o
        _   -> traceError "Expected Address Is Missing"

    checkOutputAddress :: Address -> Bool
    checkOutputAddress addr = if addr == (txOutAddress $ outputAddress addr) then True else False

    nftToFirstPlayer :: Bool
    nftToFirstPlayer = assetClassValueOf (valuePaidTo info firstPlayerPaymentPKH) (stateNFT gp) == 1

    adaToFirstPlayer :: Bool
    adaToFirstPlayer = valuePaidTo info firstPlayerPaymentPKH `geq` Ada.lovelaceValueOf firstPlayerAdaForNFT


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
