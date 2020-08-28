{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module Gateway where

import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.ByteString            (ByteString)
import           Data.ByteString.Char8      (pack)
import           Data.List
import           Data.Maybe
import           Data.Proxy
import           Data.Text.IO               as T (readFile, writeFile)

import           GHC.Generics
import           Language.Javascript.JQuery
import           Network.HTTP.Client        hiding (Proxy)
import           Network.Wai
import           Network.Wai.Handler.Warp

import           Servant.API
import           Servant.API.Empty
import           Servant.Client
import           Servant.Server
import           Servant.JS
import           Servant.Server.StaticFiles

import           Data.Attoparsec.Text
import           Debug.Trace

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Trans.Reader

import qualified Data.Map                   as Map
import qualified Data.Text as T (Text)

import           Data.Text.Encoding         (decodeUtf8)
import           Network.HTTP.Client        (defaultManagerSettings, newManager)

import           System.Random

import           Config

type GatewayApi = "user" :> Get '[JSON] User 
                  :<|> "board" :> Get '[JSON] Board
                  :<|> "turn" :> Get '[JSON] Bool
                  :<|> "alternative" :> Get '[JSON] Board
                  :<|> "allowed" :> Get '[JSON] [Int]
                  :<|> "roll" :> Get '[JSON] Int
                  :<|> "move" :> (Capture "pawn" Int :> Post '[JSON] String
                                  :<|> "skip" :> Post '[JSON] String)

gatewayApi :: Proxy GatewayApi
gatewayApi = Proxy

server :: Manager -> Player -> Server GatewayApi
server manager player =
  let
    user = fromJust $ Map.lookup player userByName
  in
    return user
    :<|> getBoardServer manager user
    :<|> getTurnServer manager user
    :<|> getAlternativeServer manager user
    :<|> getAllowedServer manager user
    :<|> getRollServer manager user
    :<|> postMoveServer manager user 
    :<|> postSkipServer manager user


getBoardServer :: Manager -> User -> Handler Board
getBoardServer manager user = do let port = timeToPort $ uTime user
                                     url = BaseUrl Http "localhost" port "/state"
                                 state <- liftIO $ runClientM (client getApi) (mkClientEnv manager url)
                                 either microserviceError (return . getBoard) state

getTurnServer :: Manager -> User -> Handler Bool
getTurnServer manager user = do let port = timeToPort $ uTime user
                                    url = BaseUrl Http "localhost" port "/state"
                                state <- liftIO $ runClientM (client getApi) (mkClientEnv manager url)
                                either microserviceError (return . (== uTeam user) . gsTurn) state

getAlternativeServer :: Manager -> User -> Handler Board
getAlternativeServer manager user = do let port = timeToPort $ swapTime $ uTime user
                                           url = BaseUrl Http "localhost" port "/state"
                                       state <- liftIO $ runClientM (client getApi) (mkClientEnv manager url)
                                       either microserviceError (return . getBoard) state

getAllowedServer :: Manager -> User -> Handler [Int]
getAllowedServer manager user = do let port = timeToPort $ uTime user
                                       team = uTeam user
                                       url = BaseUrl Http "localhost" port ("/allowed/" ++ show team)
                                   allowed <- liftIO $ runClientM (client getApi) (mkClientEnv manager url)
                                   either microserviceError return allowed

getRollServer :: Manager -> User -> Handler Int
getRollServer manager user = do let team = uTeam user
                                    urlAllowed = BaseUrl Http "localhost" (timeToPort Past) ("/allowed/" ++ show team)
                                allowed <- liftIO $ runClientM (client getApi :: ClientM [Int]) (mkClientEnv manager urlAllowed)
                                case allowed of 
                                  Left e -> microserviceError e
                                  Right [] -> do let url = BaseUrl Http "localhost" rollPort "/roll/generate"
                                                 liftIO $ runClientM (client postApi :: ClientM Int) (mkClientEnv manager url)
                                                 return 6
                                  Right _ ->  do let url = BaseUrl Http "localhost" rollPort "/roll"
                                                 roll <- liftIO $ runClientM (client getApi) (mkClientEnv manager url)
                                                 either microserviceError return roll

postMoveServer :: Manager -> User -> Int -> Handler String
postMoveServer manager user pawn = do let port = timeToPort $ uTime user
                                          team = uTeam user
                                          url = BaseUrl Http "localhost" port ("/move/" ++ show team ++ "/" ++ show pawn)
                                      state <- liftIO $ runClientM (client postApi) (mkClientEnv manager url)
                                      either microserviceError return state

postSkipServer :: Manager -> User -> Handler String
postSkipServer manager user = do let port = timeToPort $ uTime user
                                     team = uTeam user
                                     url = BaseUrl Http "localhost" port "/move/skip"
                                 state <- liftIO $ runClientM (client postApi) (mkClientEnv manager url)
                                 either microserviceError return state


-- JQuery generation

apiJS1 :: T.Text
apiJS1 = jsForAPI gatewayApi jquery

writeJSFiles :: IO ()
writeJSFiles = do
  T.writeFile "static/api.js" apiJS1
  jq <- Language.Javascript.JQuery.file >>= T.readFile
  T.writeFile "static/jq.js" jq
