{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

import  Ledger              ( POSIXTime(POSIXTime), Slot(Slot) )
import  Ledger.TimeSlot     ( SlotConfig(SlotConfig), posixTimeToEnclosingSlot )
import  System.Environment  (getArgs)

beginningOfTestnetTime :: POSIXTime
beginningOfTestnetTime = POSIXTime 1655683180000

testnetConf :: SlotConfig
testnetConf = SlotConfig 1000 beginningOfTestnetTime

posixTimeToSlot :: POSIXTime -> Slot
posixTimeToSlot time = posixTimeToEnclosingSlot testnetConf time

main :: IO ()
main = do
    [time] <- getArgs
    let time' = POSIXTime $ read time
    putStrLn "\n<---------------DONE---------------->\n"
    print $ posixTimeToSlot time'
    putStrLn "\n<----------------------------------->"
