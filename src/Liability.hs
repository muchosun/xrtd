{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Liability (create, finalize) where

import           Data.Default                     (def)
import           Data.Either                      (fromRight)
import           Data.Monoid                      ((<>))
import qualified Data.Proxy                       as Proxy
import           Network.Ethereum.ABI.Codec       (encode')
import           Network.Ethereum.Contract.Method
import           Network.Ethereum.Web3
import qualified Network.Ethereum.Web3.Eth        as Eth
import           Network.Ethereum.Web3.Types      (Hash)
import           Pipes

import qualified Contract.Factory                 as Factory
import qualified Contract.Liability               as Liability
import qualified Contract.Lighthouse              as Lighthouse
import           Message

lighthouse :: Call
lighthouse = def { callTo = Just "0x532975D56cf18F597480e2521246B273aD9AE348" }

create :: Pipe (Ask, Bid) Hash Web3 ()
create = do
    (ask, bid) <- await
    (account:_) <- lift Eth.accounts
    tx <- lift $ Factory.createLiability (lighthouse { callFrom = Just account }) (encode' ask) (encode' bid)
    yield tx

finalize :: Pipe Report Hash Web3 ()
finalize = do
    Report{..} <- await
    (account:_) <- lift Eth.accounts
    let l = lighthouse { callFrom = Just account }
        finalize = Liability.FinalizeData reportResult reportSignature reportAgree
        selFinalize = selector (Proxy.Proxy :: Proxy.Proxy Liability.FinalizeData)
    tx <- lift $ Lighthouse.to l reportLiability (selFinalize <> encode' finalize)
    yield tx
