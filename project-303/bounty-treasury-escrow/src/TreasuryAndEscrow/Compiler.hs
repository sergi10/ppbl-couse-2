{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImportQualifiedPost #-}

module TreasuryAndEscrow.Compiler where

import Cardano.Api
import Cardano.Api.Shelley (PlutusScript (..))
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Ledger

import TreasuryAndEscrow.EscrowValidator as Escrow
import TreasuryAndEscrow.TreasuryValidator as Treasury
import TreasuryAndEscrow.Types

writeValidator :: FilePath -> Ledger.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Ledger.unValidatorScript

writeBountyEscrowScript :: IO (Either (FileError ()) ())
writeBountyEscrowScript = writeValidator "output/example-bounty-escrow-new-preprod.plutus" $ Escrow.validator $ BountyParam
    {
      bountyTokenPolicyId     = "fb45417ab92a155da3b31a8928c873eb9fd36c62184c736f189d334c"
    , bountyTokenName         = "tGimbal"
    , accessTokenPolicyId     = "738ec2c17e3319fa3e3721dbd99f0b31fce1b8006bb57fbd635e3784"
    , treasuryIssuerPolicyId  = "94784b7e88ae2a6732dc5c0f41b3151e5f9719ea513f19cdb9aecfb3"
    }

writeBountyTreasuryScript :: IO (Either (FileError ()) ())
writeBountyTreasuryScript = writeValidator "output/example-bounty-treasury-new-preprod.plutus" $ Treasury.validator $ TreasuryParam
    {
      tAccessTokenPolicyId    = "738ec2c17e3319fa3e3721dbd99f0b31fce1b8006bb57fbd635e3784"
    , tIssuerTokenPolicyId    = "94784b7e88ae2a6732dc5c0f41b3151e5f9719ea513f19cdb9aecfb3"
    , bountyContractHash      = ""
    , tBountyTokenPolicyId    = "fb45417ab92a155da3b31a8928c873eb9fd36c62184c736f189d334c"
    , tBountyTokenName        = "tGimbal"
    }
