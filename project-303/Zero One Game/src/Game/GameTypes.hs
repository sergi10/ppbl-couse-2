{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies #-}

module  Game.GameTypes
    ( GameParams (..)
    , PlayerChoice (..)
    , GameDatum (..)
    , GameRedeemer (..)
    , GameTypes
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

data GameParams = GameParams
    {   firstPlayerAddress  ::  !Address
    ,   secondPlayerAddress ::  !Address
    ,   gBet                ::  !Integer
    ,   gPlayDeadline       ::  !POSIXTime
    ,   gRevealDeadline     ::  !POSIXTime
    ,   stateNFT            ::  !AssetClass
    }   deriving (Pr.Eq, Pr.Ord, Show, Generic, ToJSON, FromJSON)

PlutusTx.makeLift ''GameParams

data PlayerChoice = Zero | One
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Pr.Eq, Pr.Ord, Pr.Read)

instance Eq PlayerChoice where
    {-# INLINABLE (==) #-}
    Zero == Zero = True
    One  == One  = True
    _    == _    = False

PlutusTx.unstableMakeIsData ''PlayerChoice
PlutusTx.makeLift ''PlayerChoice

data GameDatum = GameDatum BuiltinByteString (Maybe PlayerChoice)
    deriving (Pr.Eq, Pr.Ord, Show, Generic, ToJSON, FromJSON, ToSchema)

instance Eq GameDatum where
    {-# INLINABLE (==) #-}
    GameDatum bs mc == GameDatum bs' mc' = (bs == bs') && (mc == mc')

PlutusTx.unstableMakeIsData ''GameDatum
PlutusTx.makeLift ''GameDatum

data GameRedeemer = Play PlayerChoice | Reveal BuiltinByteString | ClaimByFirstPlayer | ClaimBySecondPlayer
    deriving Show

PlutusTx.unstableMakeIsData ''GameRedeemer

data GameTypes
instance ValidatorTypes GameTypes where
    type instance DatumType GameTypes = GameDatum
    type instance RedeemerType GameTypes = GameRedeemer
