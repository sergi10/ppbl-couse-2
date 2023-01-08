{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}

import            Cardano.Api
import            Data.Aeson                    (encode)
import  qualified Data.ByteString.Lazy  as LBS
import            PlutusTx                      (Data (..))
import  qualified PlutusTx

import           TreasuryAndEscrow.Types

datum :: BountyEscrowDatum
datum = BountyEscrowDatum
  { bedContributorPkh     = "e02f8dd57e378ee673d5bf3bf7228382f131b1767d588a79cde2726a"
  , bedLovelaceAmount     = 25000000
  , bedTokenAmount        = 250
  , bedExpirationTime     = 1675987200000
  }

redeemer :: BountyAction
redeemer = Distribute

dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

main :: IO ()
main = do
      writeJSON "output/Sergi10_BountyEscrow-Datum.json" datum
      writeJSON "output/Sergi10_BountyEscrow-Redeemer.json" redeemer
      putStrLn "\n<---------------DONE--------------->"
      putStrLn "\nATTENTION: JSON flies were written at 'output' directory"
      putStrLn "\n<----------------------------------->"
