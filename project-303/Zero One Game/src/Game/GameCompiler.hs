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

{--------------------FIRST PLAYER CREDENTIALS--------------------}

firstPlayerPaymentPKH :: PaymentPubKeyHash
firstPlayerPaymentPKH = PaymentPubKeyHash "a7a098e74a8937820091e0a6f20f81f2ff54b3d1709b597a9e8b8979"

firstPlayerStakePKH :: StakePubKeyHash
firstPlayerStakePKH = StakePubKeyHash "fb6455278035fb628e4afa453fecfa13ee6eb8812a4fafd1b6f1d4e9"

{----------------------------------------------------------------}

{-------------------SECOND PLAYER CREDENTIALS--------------------}

secondPlayerPaymentPKH :: PaymentPubKeyHash
secondPlayerPaymentPKH = PaymentPubKeyHash "c354d0063899c42228ad3ff8f520b5af0fc73d86984367f7a2d7217f"

secondPlayerStakePKH :: StakePubKeyHash
secondPlayerStakePKH = StakePubKeyHash "bae4d072c379d613a3557cada090a901b069b0185de3e41ddcd92d86"

{----------------------------------------------------------------}

{--------------------------GAME DETAILS--------------------------}

gameBetAmount :: Integer
gameBetAmount = 3000000

gamePlayDeadline :: POSIXTime
gamePlayDeadline = 1662929033000

firstPlayerRevealDeadline :: POSIXTime
firstPlayerRevealDeadline = 1662925433000

{----------------------------------------------------------------}

{------------------------STATE NFT DETAILS-----------------------}

nftCurrencySymbol :: CurrencySymbol
nftCurrencySymbol = "e95879b77e864364ac1974015ae8d28837c44f67a18e6a0bf95cd671"

nftTokenName :: TokenName
nftTokenName = "project303StateNFT"

{----------------------------------------------------------------}

writeZeroOneGameScript :: IO (Either (FileError ()) ())
writeZeroOneGameScript = writeValidator "output/ZeroOneGame.plutus" $ Game.ZeroOneGame.gameValidator $ GameParams
    { firstPlayerAddress   = pubKeyHashAddress firstPlayerPaymentPKH (Just firstPlayerStakePKH)
    , secondPlayerAddress  = pubKeyHashAddress secondPlayerPaymentPKH (Just secondPlayerStakePKH)
    , gBet                 = gameBetAmount
    , gPlayDeadline        = gamePlayDeadline
    , gRevealDeadline      = firstPlayerRevealDeadline
    , stateNFT             = AssetClass (nftCurrencySymbol, nftTokenName)
    }
