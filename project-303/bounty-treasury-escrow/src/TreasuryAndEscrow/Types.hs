{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies #-}

module TreasuryAndEscrow.Types
    ( TreasuryParam (..)
    , TreasuryDatum (..)
    , BountyDetails (..)
    , TreasuryAction (..)
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
    { tIssuerTokenPolicyId   :: !CurrencySymbol
    , tAccessTokenPolicyId   :: !CurrencySymbol
    , bountyContractHash     :: !ValidatorHash
    , tBountyTokenPolicyId   :: !CurrencySymbol
    , tBountyTokenName       :: !TokenName
    } deriving (Pr.Eq, Pr.Ord, Show, Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''TreasuryParam

-- Issuer is identified with an Issuer Token
-- Upcoming bounty: update contract so that issuerTokenNames is a list of TokenName]
data TreasuryDatum = TreasuryDatum
  { bountyCount     :: !Integer
  , issuerTokenName :: !TokenName
  } deriving (Pr.Eq, Pr.Ord, Show, Generic)

PlutusTx.unstableMakeIsData ''TreasuryDatum

data BountyDetails = BountyDetails
  { contributorPkh      :: !PubKeyHash
  , lovelaceAmount      :: !Integer
  , tokenAmount         :: !Integer
  , expirationTime      :: !POSIXTime
  } deriving (Pr.Eq, Pr.Ord, Show, Generic, ToJSON, FromJSON, ToSchema)

instance Eq BountyDetails where
  {-# INLINABLE (==) #-}
  BountyDetails  cP lA tA eT == BountyDetails cP' lA' tA' eT' =
    (cP == cP') && (lA == lA') && (tA == tA') && (eT == eT')

PlutusTx.unstableMakeIsData ''BountyDetails
PlutusTx.makeLift ''BountyDetails

data TreasuryAction = Commit BountyDetails | Manage
    deriving Show

PlutusTx.makeIsDataIndexed ''TreasuryAction [('Commit, 0), ('Manage, 1)]
PlutusTx.makeLift ''TreasuryAction

data TreasuryTypes
instance ValidatorTypes TreasuryTypes where
    type DatumType TreasuryTypes = TreasuryDatum
    type RedeemerType TreasuryTypes = TreasuryAction

-- BountyEscrow
-- INLINABLE to use On Chain
{-# INLINABLE escrowDatum #-}
escrowDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe BountyEscrowDatum
escrowDatum o f = do
    dh <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d

data BountyEscrowDatum = BountyEscrowDatum
  { bedContributorPkh      :: !PubKeyHash
  , bedLovelaceAmount      :: !Integer
  , bedTokenAmount         :: !Integer
  , bedExpirationTime      :: !POSIXTime
  } deriving (Pr.Eq, Pr.Ord, Show, Generic, ToJSON, FromJSON, ToSchema)

instance Eq BountyEscrowDatum where
  {-# INLINABLE (==) #-}
  BountyEscrowDatum bCP bLA bTA bET == BountyEscrowDatum bCP' bLA' bTA' bET' =
    (bCP == bCP') && (bLA == bLA') && (bTA == bTA') && (bET == bET')

    -- Alternative way of comparisons
    -- a == b = (bedContributorPkh  a == bedContributorPkh b) &&
    --          (bedLovelaceAmount  a == bedLovelaceAmount b) &&
    --          (bedTokenAmount     a == bedTokenAmount    b) &&
    --          (bedExpirationTime  a == bedExpirationTime b)

PlutusTx.unstableMakeIsData ''BountyEscrowDatum
PlutusTx.makeLift ''BountyEscrowDatum

data BountyParam = BountyParam
    { bountyTokenPolicyId     :: !CurrencySymbol
    , bountyTokenName         :: !TokenName
    , accessTokenPolicyId     :: !CurrencySymbol
    , treasuryIssuerPolicyId  :: !CurrencySymbol
    } deriving (Pr.Eq, Pr.Ord, Show, Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''BountyParam

data BountyAction = Cancel | Distribute | Update
  deriving Show

PlutusTx.makeIsDataIndexed ''BountyAction [('Cancel, 0), ('Distribute, 1), ('Update, 2)]
PlutusTx.makeLift ''BountyAction

data EscrowTypes
instance ValidatorTypes EscrowTypes where
    type DatumType EscrowTypes = BountyEscrowDatum
    type RedeemerType EscrowTypes = BountyAction
