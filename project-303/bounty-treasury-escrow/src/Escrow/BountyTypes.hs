{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies #-}

module Escrow.BountyTypes
    ( TreasuryParam (..)
    , WithdrawalDatum (..)
    , BountyDetails (..)
    , BountyAction (..)
    , BountyParam (..)
    , BountyEscrowDatum (..)
    , TreasuryTypes
    , EscrowTypes
    , escrowDatum
    ) where

import              Data.Aeson                  (ToJSON, FromJSON)
import              GHC.Generics                (Generic)
import              Schema                      (ToSchema)
import              Ledger              hiding  (singleton)
import              Ledger.Typed.Scripts
import  qualified   PlutusTx
import              PlutusTx.Prelude    hiding  (Semigroup (..), unless)
import              Prelude                     (Show (..))
import  qualified   Prelude                 as  Pr

data TreasuryParam = TreasuryParam
    { tAccessTokenPolicyId   :: !CurrencySymbol
    , bountyContractHash     :: !ValidatorHash
    , tBountyTokenPolicyId   :: !CurrencySymbol
    , tBountyTokenName       :: !TokenName
    , tTreasuryIssuerPkh     :: !PubKeyHash
    } deriving (Pr.Eq, Pr.Ord, Show, Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''TreasuryParam

data WithdrawalDatum = WithdrawalDatum
  { bountyCount     :: !Integer
  , treasuryKey     :: !PubKeyHash
  } deriving (Pr.Eq, Pr.Ord, Show, Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''WithdrawalDatum

data TreasuryTypes
instance ValidatorTypes TreasuryTypes where
    type DatumType TreasuryTypes = WithdrawalDatum
    type RedeemerType TreasuryTypes = BountyDetails

data BountyDetails = BountyDetails
  { issuerPkh           :: !PubKeyHash
  , contributorPkh      :: !PubKeyHash
  , lovelaceAmount      :: !Integer
  , tokenAmount         :: !Integer
  , expirationTime      :: !POSIXTime
  } deriving (Pr.Eq, Pr.Ord, Show, Generic, ToJSON, FromJSON, ToSchema)

instance Eq BountyDetails where
  {-# INLINABLE (==) #-}
  BountyDetails iP cP lA tA eT == BountyDetails iP' cP' lA' tA' eT' =
    (iP == iP') && (cP == cP') && (lA == lA') && (tA == tA') && (eT == eT')

    -- Alternative way of comparisons
    -- a == b = (issuerPkh       a == issuerPkh      b) &&
    --          (contributorPkh  a == contributorPkh b) &&
    --          (lovelaceAmount  a == lovelaceAmount b) &&
    --          (expirationTime  a == expirationTime b)

PlutusTx.unstableMakeIsData ''BountyDetails
PlutusTx.makeLift ''BountyDetails

-- BountyEscrow
-- INLINABLE to use On Chain
{-# INLINABLE escrowDatum #-}
escrowDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe BountyEscrowDatum
escrowDatum o f = do
    dh <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d

data BountyEscrowDatum = BountyEscrowDatum
  { bedIssuerPkh           :: !PubKeyHash
  , bedContributorPkh      :: !PubKeyHash
  , bedLovelaceAmount      :: !Integer
  , bedTokenAmount         :: !Integer
  , bedExpirationTime      :: !POSIXTime
  } deriving (Pr.Eq, Pr.Ord, Show, Generic, ToJSON, FromJSON, ToSchema)

instance Eq BountyEscrowDatum where
  {-# INLINABLE (==) #-}
  BountyEscrowDatum bIP bCP bLA bTA bET == BountyEscrowDatum bIP' bCP' bLA' bTA' bET' =
    (bIP == bIP') && (bCP == bCP') && (bLA == bLA') && (bTA == bTA') && (bET == bET')

    -- Alternative way of comparisons
    -- a == b = (bedIssuerPkh       a == bedIssuerPkh      b) &&
    --          (bedContributorPkh  a == bedContributorPkh b) &&
    --          (bedLovelaceAmount  a == bedLovelaceAmount b) &&
    --          (bedTokenAmount     a == bedTokenAmount    b) &&
    --          (bedExpirationTime  a == bedExpirationTime b)

PlutusTx.unstableMakeIsData ''BountyEscrowDatum
PlutusTx.makeLift ''BountyEscrowDatum

data BountyParam = BountyParam
    { bountyTokenPolicyId     :: !CurrencySymbol
    , bountyTokenName         :: !TokenName
    , accessTokenPolicyId     :: !CurrencySymbol
    , treasuryIssuerPkh       :: !PubKeyHash
    } deriving (Pr.Eq, Pr.Ord, Show, Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''BountyParam

data BountyAction = Cancel | Update | Distribute
  deriving Show

PlutusTx.makeIsDataIndexed ''BountyAction [('Cancel, 0), ('Update, 1), ('Distribute, 2)]
PlutusTx.makeLift ''BountyAction

data EscrowTypes
instance ValidatorTypes EscrowTypes where
    type DatumType EscrowTypes = BountyEscrowDatum
    type RedeemerType EscrowTypes = BountyAction
