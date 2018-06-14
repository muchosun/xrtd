{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
module Message (
    EthHash(..)
  , EthSignature(..)
  , Ask(..)
  , Bid(..)
  , Report(..)
  ) where

import           Crypto.Hash                (Digest, Keccak_256)
import qualified Crypto.Hash                as Crypto
import qualified Data.ByteArray             as BA
import           Data.Monoid                ((<>))
import           Generics.SOP               (Generic)
import qualified GHC.Generics               as GHC
import           Network.Ethereum.ABI.Codec (encode)
import           Network.Ethereum.Web3
import qualified Network.Ethereum.Web3.Eth  as Eth

class EthHash a where
    hash :: a -> Digest Keccak_256

class EthHash a => EthSignature a where
    sign :: a -> Web3 Bytes
    sign a = do (account : _) <- Eth.accounts
                Eth.sign account (BA.convert $ hash a)

data Ask = Ask
    { askModel        :: !Bytes
    , askObjective    :: !Bytes
    , askToken        :: !Address
    , askCost         :: !(UIntN 256)
    , askValidator    :: !Address
    , askValidatorFee :: !(UIntN 256)
    , askDeadline     :: !(UIntN 256)
    , askNonce        :: !(BytesN 32)
    , askSignature    :: !Bytes
    }
  deriving (Show, GHC.Generic)

instance EthHash Ask where
    hash = askHash

instance EthSignature Ask
instance Generic Ask

data Bid = Bid
    { bidModel         :: !Bytes
    , bidObjective     :: !Bytes
    , bidToken         :: !Address
    , bidCost          :: !(UIntN 256)
    , bidLighthouseFee :: !(UIntN 256)
    , bidDeadline      :: !(UIntN 256)
    , bidNonce         :: !(BytesN 32)
    , bidSignature     :: !Bytes
    }
  deriving (Show, GHC.Generic)

instance EthHash Bid where
    hash = bidHash

instance EthSignature Bid
instance Generic Bid

data Report = Report
  { reportLiability :: !Address
  , reportResult    :: !Bytes
  , reportSignature :: !Bytes
  , reportAgree     :: !Bool
  } deriving (Show, GHC.Generic)

instance EthHash Report where
    hash = resHash

instance EthSignature Report
instance Generic Report

askHash :: Ask -> Digest Keccak_256
askHash Ask{..} = Crypto.hash $ askModel
                             <> askObjective
                             <> BA.drop 12 (encode askToken)
                             <> encode askCost
                             <> BA.drop 12 (encode askValidator)
                             <> encode askValidatorFee
                             <> encode askDeadline
                             <> encode askNonce

bidHash :: Bid -> Digest Keccak_256
bidHash Bid{..} = Crypto.hash $ bidModel
                             <> bidObjective
                             <> BA.drop 12 (encode bidToken)
                             <> encode bidCost
                             <> encode bidLighthouseFee
                             <> encode bidDeadline
                             <> encode bidNonce

resHash :: Report -> Digest Keccak_256
resHash Report{..} = Crypto.hash $ BA.drop 12 (encode reportLiability)
                                <> reportResult
