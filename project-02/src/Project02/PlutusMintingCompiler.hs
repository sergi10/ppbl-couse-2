{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Project02.PlutusMintingCompiler where

import Cardano.Api
import Cardano.Api.Shelley (PlutusScript (..))
import Codec.Serialise (serialise)
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Ledger
import Plutus.V1.Ledger.Api (toData)
import PlutusTx (Data (..), ToData)
import qualified Project02.PlutusMintingScript as Minter

writeValidator :: FilePath -> Ledger.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Ledger.unValidatorScript

writeMintingValidatorScript :: IO (Either (FileError ()) ())
writeMintingValidatorScript = writeValidator "output/my-minting-script.plutus" Minter.validator