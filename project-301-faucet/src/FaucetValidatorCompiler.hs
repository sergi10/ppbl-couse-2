{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module FaucetValidatorCompiler where

import Cardano.Api
import Cardano.Api.Shelley (PlutusScript (..))
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Ledger
import qualified FaucetValidatorScriptMinimumLogic (validator, FaucetParams (..))


writeValidator :: FilePath -> Ledger.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Ledger.unValidatorScript

-- This is a comment in Haskell
-- 1. Change the name of the output file to ppbl-faucet-yourTokenName.plutus
-- 2. Change the faucetTokenSymbol and faucetTokenName to match the token that you want to distribute
-- 3. Change the withdrawalAmount to whatever amount you want people to able to claim in one transaction
-- 4. Compile your script
-- 5. Recommended: take a screenshot of this file so that you can remember the parameters you used

writeFaucetScript :: IO (Either (FileError ()) ())
writeFaucetScript = writeValidator "output/ppbl-faucet-preprod-minimum.plutus" $ FaucetValidatorScriptMinimumLogic.validator $ FaucetValidatorScriptMinimumLogic.FaucetParams
    {
      FaucetValidatorScriptMinimumLogic.accessTokenSymbol     = "748ee66265a1853c6f068f86622e36b0dda8edfa69c689a7dd232c60"
    , FaucetValidatorScriptMinimumLogic.accessTokenName       = "PPBLSummer2022"
    , FaucetValidatorScriptMinimumLogic.faucetTokenSymbol     = "fb45417ab92a155da3b31a8928c873eb9fd36c62184c736f189d334c"
    , FaucetValidatorScriptMinimumLogic.faucetTokenName       = "tgimbal"
    , FaucetValidatorScriptMinimumLogic.withdrawalAmount      = 250
    }