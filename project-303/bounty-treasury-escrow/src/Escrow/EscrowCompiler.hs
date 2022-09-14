{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Escrow.EscrowCompiler where

import Cardano.Api
import Cardano.Api.Shelley (PlutusScript (..))
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Ledger

import Escrow.BountyEscrow
import Escrow.BountyTreasury
import Escrow.BountyTypes

writeValidator :: FilePath -> Ledger.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Ledger.unValidatorScript

writeBountyEscrowScript :: IO (Either (FileError ()) ())
writeBountyEscrowScript = writeValidator "output/jd-bounty-escrow.plutus" $ Escrow.BountyEscrow.validator $ BountyParam
    {
      bountyTokenPolicyId = "fb45417ab92a155da3b31a8928c873eb9fd36c62184c736f189d334c"
    , bountyTokenName     = "tgimbal"
    , accessTokenPolicyId = "738ec2c17e3319fa3e3721dbd99f0b31fce1b8006bb57fbd635e3784"
    , treasuryIssuerPkh   = "65295d6feacfc33fe029f51785770d92373e82cde28c3cd8c55a3cd1"
    }

writeBountyTreasuryScript :: IO (Either (FileError ()) ())
writeBountyTreasuryScript = writeValidator "output/jd-bounty-treasury.plutus" $ Escrow.BountyTreasury.validator $ TreasuryParam
    {
      tAccessTokenPolicyId = "738ec2c17e3319fa3e3721dbd99f0b31fce1b8006bb57fbd635e3784"
    , bountyContractHash   = "3fceb0fdad63e0e54488da98b9e87c804b148a617af80aa1ad50fdea"
    , tBountyTokenPolicyId = "fb45417ab92a155da3b31a8928c873eb9fd36c62184c736f189d334c"
    , tBountyTokenName     = "tgimbal"
    , tTreasuryIssuerPkh   = "65295d6feacfc33fe029f51785770d92373e82cde28c3cd8c55a3cd1"
    }
