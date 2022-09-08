{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module FaucetValidatorCompiler where

import Cardano.Api
import Cardano.Api.Shelley (PlutusScript (..))
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Ledger
import qualified FaucetValidatorScriptWithPkh (validator, FaucetParams (..))


writeValidator :: FilePath -> Ledger.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Ledger.unValidatorScript

-- This is a comment in Haskell
-- 1. Change the name of the output file to ppbl-faucet-yourTokenName.plutus
-- 2. Change the faucetTokenSymbol and faucetTokenName to match the token that you want to distribute
-- 3. Change the withdrawalAmount to whatever amount you want people to able to claim in one transaction
-- 4. Compile your script
-- 5. Recommended: take a screenshot of this file so that you can remember the parameters you used

writeFaucetScript :: IO (Either (FileError ()) ())
writeFaucetScript = writeValidator "output/ppbl-faucet-mainnet-gimbal.plutus" $ FaucetValidatorScriptWithPkh.validator $ FaucetValidatorScriptWithPkh.FaucetParams
    {
      FaucetValidatorScriptWithPkh.accessTokenSymbol     = "28adc4b12edd23bad18823c0b0a74b24a95ccf45babf8a3782217f4f"
    , FaucetValidatorScriptWithPkh.accessTokenName       = "PPBLContribLevel1"
    , FaucetValidatorScriptWithPkh.faucetTokenSymbol     = "2b0a04a7b60132b1805b296c7fcb3b217ff14413991bf76f72663c30"
    , FaucetValidatorScriptWithPkh.faucetTokenName       = "gimbal"
    , FaucetValidatorScriptWithPkh.withdrawalAmount      = 25000000
    }