{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module FaucetValidatorCompiler where

import Cardano.Api
import Cardano.Api.Shelley (PlutusScript (..))
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Ledger
import qualified FaucetValidatorScriptInteger (validator, FaucetParams (..))


writeValidator :: FilePath -> Ledger.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Ledger.unValidatorScript

writeFaucetScript :: IO (Either (FileError ()) ())
writeFaucetScript = writeValidator "output/ppbl-faucet-integer.plutus" $ FaucetValidatorScriptInteger.validator $ FaucetValidatorScriptInteger.FaucetParams
    {
      FaucetValidatorScriptInteger.accessTokenSymbol     = "1309921891e459c7e9acb338d5dae18f98d1c2f55c1852cd5cf341f9"
    , FaucetValidatorScriptInteger.accessTokenName       = "PPBLSummer2022"
    , FaucetValidatorScriptInteger.faucetTokenSymbol     = "6c57132fde399c9ea6e462c4214d164984891087922b6fa2472b175b"
    , FaucetValidatorScriptInteger.faucetTokenName       = "tpblTestGimbal"
    , FaucetValidatorScriptInteger.withdrawalAmount      = 5000
    }