{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

import            Cardano.Api (ScriptDataJsonSchema (ScriptDataJsonDetailedSchema), scriptDataToJson)
import            Data.Aeson  (encode)
import  qualified Data.ByteString.Lazy  as LBS
import  qualified PlutusTx
import            PlutusTx.Builtins.Internal
import            Cardano.Api.Shelley (fromPlutusData)

import           Game.GameTypes

nonce :: BuiltinByteString
nonce = "0123456789abcdef__"

playerChoice :: BuiltinByteString
playerChoice = "1"

hash :: BuiltinByteString
hash = sha2_256 $ nonce `appendByteString` playerChoice

datum :: GameDatum
datum = GameDatum hash $ Just One

redeemer :: GameRedeemer
redeemer = ClaimByFirstPlayer

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . fromPlutusData . PlutusTx.toData

main :: IO ()
main = do
      writeJSON "output/ZeroOneGame-Datum.json" datum
      writeJSON "output/ZeroOneGame-Redeemer.json" redeemer
      putStrLn "\n<---------------DONE--------------->"
      putStrLn "\nATTENTION: JSON flies were written at 'output' directory"
      putStrLn "\n<----------------------------------->"
