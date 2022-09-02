{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImportQualifiedPost #-}

module  Game.GameCompiler   where

import              Cardano.Api
import              Cardano.Api.Shelley             (PlutusScript (..))
import              Codec.Serialise                 (serialise)
import  qualified   Data.ByteString.Lazy    as  LBS
import  qualified   Data.ByteString.Short   as  SBS
import              Ledger
import              Ledger.Value

import              Game.ZeroOneGame
import              Game.GameTypes


writeValidator :: FilePath -> Ledger.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Ledger.unValidatorScript

firstPlayerPKH :: PubKeyHash
firstPlayerPKH ="a7a098e74a8937820091e0a6f20f81f2ff54b3d1709b597a9e8b8979"

secondPlayerPKH :: PubKeyHash
secondPlayerPKH = "c354d0063899c42228ad3ff8f520b5af0fc73d86984367f7a2d7217f"

stateNftCurrencySymbol :: CurrencySymbol
stateNftCurrencySymbol = "6c57132fde399c9ea6e462c4214d164984891087922b6fa2472b175b"

stateNftTokenName :: TokenName
stateNftTokenName = "project303stateNFT"

writeZeroOneGameScript :: IO (Either (FileError ()) ())
writeZeroOneGameScript = writeValidator "output/zero-one-game.plutus" $ Game.ZeroOneGame.gameValidator $ GameParams
    { firstPlayerPaymentPKH   = PaymentPubKeyHash firstPlayerPKH
    , secondPlayerPaymentPKH  = PaymentPubKeyHash secondPlayerPKH
    , gBet                    = 10000000
    , gPlayDeadline           = 1651252695000
    , gRevealDeadline         = 1651253000000
    , stateNFT                = AssetClass (stateNftCurrencySymbol, stateNftTokenName)
    }
