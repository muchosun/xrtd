{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Generator where

import           Control.Concurrent             (threadDelay)
import           Control.Monad
import           Control.Monad.Random
import qualified Data.ByteArray                 as BA
import qualified Data.ByteArray.Sized           as BAS
import           Data.Default                   (def)
import           Message
import           Network.Ethereum.Web3
import qualified Network.Ethereum.Web3.Eth      as Eth
import           Network.Ethereum.Web3.Provider
import           Network.Ethereum.Web3.Types    (DefaultBlock (..), Filter (..),
                                                 Quantity)
import           Pipes
import           Pipes.Concurrent

import qualified Contract.Factory               as Factory

xrt :: Address
xrt = "0x3cBAF1d511Adf5098511B5c5B39e1F1b506C1AFE"

factory :: Address
factory = "0x44CFBcb1Ca0d3df0925dDA3354E955d38d78ad6B"

randomBytes :: Int -> Rand StdGen Bytes
randomBytes n = BA.pack <$> replicateM n getRandom

randomBytes32 :: Rand StdGen (BytesN 32)
randomBytes32 = BAS.unsafeFromByteArrayAccess <$> randomBytes 32

randomAsk :: Address -> UIntN 256 -> Rand StdGen Ask
randomAsk xrt deadline =
    Ask <$> randomBytes 34
        <*> randomBytes 34
        <*> pure xrt
        <*> pure 1
        <*> pure "0x0000000000000000000000000000000000000000"
        <*> pure 0
        <*> pure deadline
        <*> randomBytes32
        <*> pure ""

randomDeal :: Producer (Ask, Bid) Web3 ()
randomDeal = forever $ do
    deadline <- fromIntegral . toWei . (100 +) <$> lift Eth.blockNumber

    ask@Ask{..} <- liftIO $ evalRandIO (randomAsk xrt deadline)

    nonce <- liftIO $ evalRandIO randomBytes32
    let bid = Bid askModel askObjective askToken askCost 0 askDeadline nonce ""

    ask_sign <- lift $ sign ask
    bid_sign <- lift $ sign bid

    yield (ask { askSignature = ask_sign }, bid { bidSignature = bid_sign })

newLiability :: Output Address -> Quantity -> Web3 ()
newLiability output block = do
    liftIO $ threadDelay 5000000
    let flt = (def :: Filter Factory.NewLiability)
            { filterAddress = Just [ factory ]
            , filterFromBlock = BlockWithNumber block
            }
    event flt $ \(Factory.NewLiability address) -> do
        lift $ runEffect $ yield address >-> toOutput output
        return ContinueEvent
    newLiability output =<< Eth.blockNumber

randomRep :: Pipe Address Report Web3 ()
randomRep = do
    res <- liftIO $ evalRandIO $ randomBytes 34
    addr <- await
    let rep = Report addr res "" False
    rep_sign <- lift $ sign rep
    yield $ rep { reportSignature = rep_sign }

randomReport :: Producer Report Web3 ()
randomReport = do
    (output, input) <- liftIO $ spawn unbounded
    lift $ forkWeb3 . newLiability output =<< Eth.blockNumber
    fromInput input >-> randomRep
