{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

import  qualified PlutusTx
import            PlutusTx.Builtins.Internal


nonce :: BuiltinByteString
nonce = "0123456789abcdef__"

playerChoice :: BuiltinByteString
playerChoice = "1"

hash :: BuiltinByteString
hash = sha2_256 $ nonce `appendByteString` playerChoice

main :: IO ()
main = do
      putStrLn "\n<---------------DONE--------------->"
      putStr "\nThe Answer: "
      print $ nonce `appendByteString` playerChoice
      putStr "\nHash Of The Answer: "
      print hash
      putStrLn "\n<----------------------------------->"
