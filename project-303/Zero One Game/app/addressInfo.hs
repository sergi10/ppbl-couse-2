{-# LANGUAGE OverloadedStrings #-}


import      Ledger
import      Plutus.V1.Ledger.Credential   (StakingCredential)
import      PlutusTx.Prelude

playerPaymentPKH :: PubKeyHash
playerPaymentPKH = "a7a098e74a8937820091e0a6f20f81f2ff54b3d1709b597a9e8b8979"

playerStakePKH :: PubKeyHash
playerStakePKH = "fb6455278035fb628e4afa453fecfa13ee6eb8812a4fafd1b6f1d4e9"

paymentPKH :: PaymentPubKeyHash
paymentPKH = PaymentPubKeyHash playerPaymentPKH

stakePKH :: StakePubKeyHash
stakePKH = StakePubKeyHash playerStakePKH

address :: Address
address = pubKeyHashAddress paymentPKH (Just stakePKH)

paymentPart :: PubKeyHash
paymentPart =  case toPubKeyHash address of
      Just p -> p
      Nothing -> traceError "No Payment Public Key Hash"

stakePart :: Maybe StakingCredential
stakePart = stakingCredential address

main :: IO ()
main = do
      putStrLn "\n<---------------DONE--------------->"
      putStr "\nThe Address: "
      print address
      putStr "\nThe Payment Public Key Hash: "
      print paymentPart
      putStr "\nThe Stake Public Key Hash: "
      print stakePart
      putStrLn "\n<----------------------------------->"
