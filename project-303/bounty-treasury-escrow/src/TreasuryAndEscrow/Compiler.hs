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
writeBountyEscrowScript = writeValidator "output/sergi10-bounty-escrow-preprod.plutus" $ Escrow.validator $ BountyParam
    {
      bountyTokenPolicyId     = "bda714dac42c0c1c8303cf1b109b18cdfd04f8a578432895ac8e1ee4"
    , bountyTokenName         = "tSergi10"
    , accessTokenPolicyId     = "bda714dac42c0c1c8303cf1b109b18cdfd04f8a578432895ac8e1ee4"
    , treasuryIssuerPolicyId  = "c09c50bf1f23530376a2bd14611c64eb0f0bd4f197a402a6688b5c70"
    }

writeBountyTreasuryScript :: IO (Either (FileError ()) ())
writeBountyTreasuryScript = writeValidator "output/sergi10-bounty-treasury-preprod.plutus" $ Treasury.validator $ TreasuryParam
    {
      tAccessTokenPolicyId    = "bda714dac42c0c1c8303cf1b109b18cdfd04f8a578432895ac8e1ee4"
    , tIssuerTokenPolicyId    = "c09c50bf1f23530376a2bd14611c64eb0f0bd4f197a402a6688b5c70"
    , bountyContractHash      = "8ac05de1e80ee206a4725d5795398e72151f8ce47b9f32a7fabe33c7"
    , tBountyTokenPolicyId    = "bda714dac42c0c1c8303cf1b109b18cdfd04f8a578432895ac8e1ee4"
    , tBountyTokenName        = "tSergi10"
    }
