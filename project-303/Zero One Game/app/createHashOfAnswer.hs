{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

import            Cardano.Api (ScriptDataJsonSchema (ScriptDataJsonDetailedSchema), scriptDataToJson)
import            Data.Aeson  (encode)
import  qualified Data.ByteString.Lazy  as LBS
import  qualified PlutusTx
import            PlutusTx.Builtins.Internal
import            Cardano.Api.Shelley (fromPlutusData)


nonce :: BuiltinByteString
nonce = "0123456789abcdef__"

playerChoice :: BuiltinByteString
playerChoice = "1"

hash :: BuiltinByteString
hash = sha2_256 $ nonce `appendByteString` playerChoice

hashToByte :: PlutusTx.ToData a => a -> LBS.ByteString
hashToByte = encode . scriptDataToJson ScriptDataJsonDetailedSchema . fromPlutusData . PlutusTx.toData

main :: IO ()
main = do
      putStrLn "\n<---------------DONE--------------->"
      putStr "\nThe Answer: "
      print $ nonce `appendByteString` playerChoice
      putStr "\nHash Of The Answer: "
      print hash
      putStr "\nBytes Representation of The Hash: "
      print $ hashToByte hash
      putStrLn "\n<----------------------------------->"
