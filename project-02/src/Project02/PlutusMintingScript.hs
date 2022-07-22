{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Project02.PlutusMintingScript where

import Ledger hiding (singleton)
import qualified Ledger.Typed.Scripts as Scripts
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)

{-# INLINEABLE mkPolicy #-}
mkPolicy :: () -> ScriptContext -> Bool
mkPolicy () _ = True

policy :: Scripts.MintingPolicy
policy = mkMintingPolicyScript $$(PlutusTx.compile [||Scripts.wrapMintingPolicy mkPolicy||])

plutusScript :: Script
plutusScript = unMintingPolicyScript policy

validator :: Validator
validator = Validator plutusScript
