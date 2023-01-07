{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

import            Cardano.Api
import            Data.Aeson                    (encode)
import  qualified Data.ByteString.Lazy  as LBS
import            PlutusTx                      (Data (..))
import  qualified PlutusTx

import           TreasuryAndEscrow.Types

datum :: BountyEscrowDatum
datum = BountyEscrowDatum
  -- { bedContributorPkh     = "e02f8dd57e378ee673d5bf3bf7228382f131b1767d588a79cde2726a"
  { bedContributorPkh     = "d739ff308ecd63b4854e077e9d1fa7fa53dcce33dfffe2b1f5ad29df"
  , bedLovelaceAmount     = 20
  , bedTokenAmount        = 25000
  -- , bedExpirationTime     = 1651252695000
  , bedExpirationTime     = 1673308800000 --10 January 2023 0:00:00
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
      writeJSON "output/BountyEscrow-Datum-Hardcoded.json" datum
      writeJSON "output/BountyEscrow-Redeemer-Hardcoded.json" redeemer
      putStrLn "\n<---------------DONE--------------->"
      putStrLn "\nATTENTION: JSON flies were written at 'output' directory"
      putStrLn "\n<----------------------------------->"
