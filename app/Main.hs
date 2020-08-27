module Main where
    
import Control.Monad.IO.Class

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Client hiding (Proxy)

import Control.Concurrent.Async 
import Control.Concurrent.STM

import Config
import Gateway
import Roll
import PastGame
import FutureGame

main :: IO ()
main = do manager1 <- newManager defaultManagerSettings
          manager2 <- newManager defaultManagerSettings 
          manager3 <- newManager defaultManagerSettings
          var1 <- liftIO $ atomically $ newTVar 6
          var2 <- liftIO $ atomically $ newTVar pastBeginState
          var3 <- liftIO $ atomically $ newTVar futureBeginState
          mapConcurrently_ id [run 8080 (gatewayApp manager1), 
                               run rollPort (rollApp var1), 
                               run (timeToPort Past) (pastApp manager2 var2),
                               run (timeToPort Future) (futureApp manager3 var3)]
