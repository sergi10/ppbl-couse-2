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
writePpblDatum = writeJson "src/Ppbl/output/MyPpblDatum.json" $ PpblDatum
  {
    sellerAddress = "d739ff308ecd63b4854e077e9d1fa7fa53dcce33dfffe2b1f5ad29df" -- preprod1.hash
  , buyerAddress  = "ca91f041b4796cbb4a3dae29686827a53a018161b7917c37d2fa2e38" -- buyer.hash
  , priceAmount   = 20000000
  , cancelFees    = 5000000
  
  }

writeValidator :: FilePath -> Ledger.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Ledger.unValidatorScript

-- writePpblScript : Used to create the Plutus Script

writePpblScript :: IO (Either (FileError ()) ())
writePpblScript = writeValidator "src/Ppbl/output/MyValidator.plutus" $ Ppbl.PpblValidator.validator $ PpblParameters
    {
      ownerAddress = "d739ff308ecd63b4854e077e9d1fa7fa53dcce33dfffe2b1f5ad29df" -- -- preprod1.hash
    , ownerCut     = 5000000
    }