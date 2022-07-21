{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Project02.MyFirstPlutusMintingCompiler where

import Cardano.Api
import Cardano.Api.Shelley              (PlutusScript (..))
import Codec.Serialise                  (serialise)
import Data.Aeson                       (encode)
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.ByteString.Short  as SBS
import qualified Ledger
import Plutus.V2.Ledger.Api             (toData)
import PlutusTx                         (Data (..), ToData)

import qualified Project02.MyFirstPlutusMintingScript as MintingScript

writeValidator :: FilePath -> Ledger.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Ledger.unValidatorScript

writeMyFirstMintingValidatorScript :: IO (Either (FileError ()) ())
writeMyFirstMintingValidatorScript = writeValidator "output/my-first-minting-script.plutus" MintingScript.validator