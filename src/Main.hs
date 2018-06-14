module Main where

import           Data.Aeson
import           Generator
import           Liability
import           Network.Ethereum.Web3
import           Pipes
import           Pipes.Prelude         as P


main :: IO ()
main = do
    runWeb3 $ do
        runEffect $ randomDeal >-> Liability.create  >-> (await >>= yield . Data.Aeson.encode) >-> P.print
        runEffect $ randomReport >-> Liability.finalize  >-> (await >>= yield . Data.Aeson.encode) >-> P.print
    return ()
