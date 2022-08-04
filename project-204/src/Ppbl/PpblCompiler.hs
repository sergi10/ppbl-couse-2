{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Ppbl.PpblCompiler where

import Cardano.Api
import Cardano.Api.Shelley (PlutusScript (..))
import Codec.Serialise (serialise)
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Ledger

import Plutus.V1.Ledger.Api (Data (B, Constr, I, List, Map), ToData, toData)

import Ppbl.PpblValidator

dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (I n) = ScriptDataNumber n
dataToScriptData (B b) = ScriptDataBytes b
dataToScriptData (Map xs) = ScriptDataMap [(dataToScriptData k, dataToScriptData v) | (k, v) <- xs]
dataToScriptData (List xs) = ScriptDataList $ fmap dataToScriptData xs

writeJson :: ToData a => FilePath -> a -> IO ()
writeJson file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . toData

writePpblDatum :: IO ()
writePpblDatum = writeJson "src/Ppbl/output/PpblDatum.json" $ PpblDatum
  {
    sellerAddress = "358b2dbffc561e85c8251af0b79534be44fdf049e66ef3e2f7aa9418"
  , buyerAddress  = "b3a5d1c826945b361a3c5236d84fd8dcbf66faa27ab10ef1535d9057"
  , priceAmount   = 20000000
  , cancelFees    = 5000000
  
  }

writeValidator :: FilePath -> Ledger.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Ledger.unValidatorScript

-- writePpblScript : Used to create the Plutus Script

writePpblScript :: IO (Either (FileError ()) ())
writePpblScript = writeValidator "src/Ppbl/output/MySecondValidator.plutus" $ Ppbl.PpblValidator.validator $ PpblParameters
    {
      ownerAddress = "9c68b6455daa791d56ba1a5c33fbc9e30c92041947a74eb82589cbe7"
    , ownerCut     = 5000000
    }