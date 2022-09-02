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
writeBountyEscrowScript = writeValidator "output/bounty-escrow.plutus" $ Escrow.BountyEscrow.validator $ BountyParam
    {
      bountyTokenPolicyId = ""
    , bountyTokenName     = ""
    , accessTokenPolicyId = ""
    , treasuryIssuerPkh   = ""
    }

writeBountyTreasuryScript :: IO (Either (FileError ()) ())
writeBountyTreasuryScript = writeValidator "output/bounty-treasury.plutus" $ Escrow.BountyTreasury.validator $ TreasuryParam
    {
      tAccessTokenPolicyId = ""
    , bountyContractHash   = ""
    , tBountyTokenPolicyId = ""
    , tBountyTokenName     = ""
    , tTreasuryIssuerPkh   = ""
    }
