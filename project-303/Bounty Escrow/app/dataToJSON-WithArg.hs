{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import            Cardano.Api                   (ScriptDataJsonSchema (ScriptDataJsonDetailedSchema), scriptDataToJson)
import            Data.Aeson            as Json (encode)
import  qualified Data.ByteString.Lazy  as LBS
import            Data.String                   (fromString)
import            Ledger                hiding  (singleton)
import            PlutusTx
import            System.Environment            (getArgs)
import            Cardano.Api.Shelley           (fromPlutusData)

import Escrow.BountyTypes

main :: IO ()
main = do
  [bedIssuerPkh', bedContributorPkh', bedLovelaceAmount', bedTokenAmount', bedExpirationTime'] <- getArgs
  let bedIssuerPkh'' = fromString bedIssuerPkh'
      bedContributorPkh'' = fromString bedContributorPkh'
      bedLovelaceAmount'' = read bedLovelaceAmount'
      bedTokenAmount'' = read bedTokenAmount'
      bedExpirationTime'' = POSIXTime $ read bedExpirationTime'
      escrowInstance = BountyEscrowDatum bedIssuerPkh'' bedContributorPkh'' bedLovelaceAmount'' bedTokenAmount'' bedExpirationTime''
  writeData "output/BountyEscrowDatum-WithArg.json" escrowInstance
  putStrLn "\n<---------------DONE--------------->"
  putStrLn "\nATTENTION: JSON flies were written at 'output' directory"
  putStrLn "\n<----------------------------------->"

writeData :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeData file isData = do
  print file
  LBS.writeFile file (toJsonString isData)

toJsonString :: PlutusTx.ToData a => a -> LBS.ByteString
toJsonString =
  Json.encode
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . fromPlutusData
    . PlutusTx.toData
