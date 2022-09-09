{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

import            Cardano.Api             (ScriptDataJsonSchema (ScriptDataJsonDetailedSchema), scriptDataToJson)
import            Data.Aeson              (encode)
import  qualified Data.ByteString.Lazy as LBS
import  qualified PlutusTx
import            PlutusTx.Builtins.Internal
import            Cardano.Api.Shelley     (fromPlutusData)

import           Game.GameTypes

nonce :: BuiltinByteString
nonce = "0123456789abcdef__"

firstPlayerChoice :: BuiltinByteString
firstPlayerChoice = "0"

hash :: BuiltinByteString
hash = sha2_256 $ nonce `appendByteString` firstPlayerChoice


{-----------------FIRST PLAYER START THE GAME--------------------}

startGameDATUM :: GameDatum
startGameDATUM = GameDatum hash $ Nothing

{----------------------------------------------------------------}

{------------------SECOND PLAYER PLAY CHOICE---------------------}

secondPlayerPlayChoiceDATUM :: GameDatum
secondPlayerPlayChoiceDATUM = GameDatum hash $ Just Zero

secondPlayerPlayChoiceREDEEMER :: GameRedeemer
secondPlayerPlayChoiceREDEEMER = Play Zero

{----------------------------------------------------------------}

{------------------FIRST PLAYER REVEAL CHOICE--------------------}

firstPlayerRevealChoiceREDEEMER :: GameRedeemer
firstPlayerRevealChoiceREDEEMER = Reveal nonce

{----------------------------------------------------------------}

{---------------------FIRST PLAYER CLAIMS------------------------}

firstPlayerClaimsREDEEMER :: GameRedeemer
firstPlayerClaimsREDEEMER = ClaimByFirstPlayer

{----------------------------------------------------------------}

{---------------------SECOND PLAYER CLAIMS-----------------------}

secondPlayerClaimsREDEEMER :: GameRedeemer
secondPlayerClaimsREDEEMER = ClaimBySecondPlayer

{----------------------------------------------------------------}


writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . fromPlutusData . PlutusTx.toData

main :: IO ()
main = do
      writeJSON "output/startGame-DATUM.json" startGameDATUM

      writeJSON "output/secondPlayerPlayChoice-DATUM.json" secondPlayerPlayChoiceDATUM
      writeJSON "output/secondPlayerPlayChoice-REDEEMER.json" secondPlayerPlayChoiceREDEEMER

      writeJSON "output/firstPlayerRevealChoice-REDEEMER.json" firstPlayerRevealChoiceREDEEMER

      writeJSON "output/firstPlayerClaims-REDEEMER.json" firstPlayerClaimsREDEEMER

      writeJSON "output/secondPlayerClaims-REDEEMER.json" secondPlayerClaimsREDEEMER

      putStrLn "\n<---------------DONE--------------->"
      putStrLn "\nATTENTION: JSON flies were written at 'output' directory"
      putStrLn "\n<----------------------------------->"
