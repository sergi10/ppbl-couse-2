{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

import  Ledger              ( POSIXTime(POSIXTime), Slot(Slot) )
import  Ledger.TimeSlot     ( SlotConfig(SlotConfig), slotToEndPOSIXTime )
import  System.Environment  (getArgs)

beginningOfTestnetTime :: POSIXTime
beginningOfTestnetTime = POSIXTime 1655683180000

testnetConf :: SlotConfig
testnetConf = SlotConfig 1000 beginningOfTestnetTime

slotToPOSIXTime :: Slot -> POSIXTime
slotToPOSIXTime slotNumber = slotToEndPOSIXTime testnetConf slotNumber

main :: IO ()
main = do
    [slotNumber] <- getArgs
    let slotNumber' = Slot $ read slotNumber
    putStrLn "\n<---------------DONE---------------->\n"
    print $ slotToPOSIXTime slotNumber'
    putStrLn "\n<----------------------------------->"
