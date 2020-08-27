{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
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

type SecureApi = BasicAuth "Gepaste inlog" Player :> GatewayApi
                 :<|> Raw

secureApi :: Proxy SecureApi
secureApi = Proxy

pswds :: [ByteString]
pswds = fmap (pack . concatMap show) [seeds !! x | key <- keys , let x = read $ concatMap show key]
  where seeds = permutations [0..9]
        keys = Data.List.take 10 $ permutations [5,2,3,7,6,0]

linkPswds :: Map.Map Player ByteString
linkPswds = Map.fromList $ zip names pswds

authCheck :: BasicAuthCheck Player
authCheck =  BasicAuthCheck $ \basicAuthData ->
  let
    p = decodeUtf8 (basicAuthPassword basicAuthData)
    u = basicAuthUsername basicAuthData
  in
    case Map.lookup u namesByte of
      Nothing -> return NoSuchUser
      Just u' -> case Map.lookup u' linkPswds of
                  Nothing -> return NoSuchUser
                  Just password -> if decodeUtf8 password == p
                                   then return (Authorized u')
                                   else return BadPassword


basicAuthServerContext :: Context (BasicAuthCheck Player ': '[])
basicAuthServerContext = authCheck :. EmptyContext

-------------------------------------------------------------------------------
-- Server
-------------------------------------------------------------------------------

server :: Manager -> Player -> Server GatewayApi
server manager player =
  let
    user = fromJust $ Map.lookup player userByName
  in
    return user
    :<|> getBoardServer user manager
    :<|> getTurnServer user manager
    :<|> getAlternativeServer user manager
    :<|> getAllowedServer user manager
    :<|> getRollServer user manager
    :<|> postMoveServer user manager 
    :<|> postSkipServer user manager


getBoardServer :: User -> Manager -> Handler Board
getBoardServer user manager = do let port = timeToPort $ uTime user
                                     url = BaseUrl Http "localhost" port "/state"
                                 state <- liftIO $ runClientM (client getApi) (mkClientEnv manager url)
                                 either microserviceError (return . getBoard) state

getTurnServer :: User -> Manager -> Handler Bool
getTurnServer user manager = do let port = timeToPort $ uTime user
                                    url = BaseUrl Http "localhost" port "/state"
                                state <- liftIO $ runClientM (client getApi) (mkClientEnv manager url)
                                either microserviceError (return . (== uTeam user) . gsTurn) state

getAlternativeServer :: User -> Manager -> Handler Board
getAlternativeServer user manager = do let port = timeToPort $ swapTime $ uTime user
                                           url = BaseUrl Http "localhost" port "/state"
                                       state <- liftIO $ runClientM (client getApi) (mkClientEnv manager url)
                                       either microserviceError (return . getBoard) state

getAllowedServer :: User -> Manager -> Handler [Int]
getAllowedServer user manager = do let port = timeToPort $ uTime user
                                       team = uTeam user
                                       url = BaseUrl Http "localhost" port ("/allowed/" ++ show team)
                                   allowed <- liftIO $ runClientM (client getApi) (mkClientEnv manager url)
                                   either microserviceError return allowed

getRollServer :: User -> Manager -> Handler Int
getRollServer user manager = do let team = uTeam user
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

postMoveServer :: User -> Manager -> Int -> Handler String
postMoveServer user manager pawn = do let port = timeToPort $ uTime user
                                          team = uTeam user
                                          url = BaseUrl Http "localhost" port ("/move/" ++ show team ++ "/" ++ show pawn)
                                      state <- liftIO $ runClientM (client postApi) (mkClientEnv manager url)
                                      either microserviceError return state

postSkipServer :: User -> Manager -> Handler String
postSkipServer user manager = do let port = timeToPort $ uTime user
                                     team = uTeam user
                                     url = BaseUrl Http "localhost" port "/move/skip"
                                 state <- liftIO $ runClientM (client postApi) (mkClientEnv manager url)
                                 either microserviceError return state


apiJS1 :: T.Text
apiJS1 = jsForAPI gatewayApi jquery

writeJSFiles :: IO ()
writeJSFiles = do
  T.writeFile "static/api.js" apiJS1
  jq <- Language.Javascript.JQuery.file >>= T.readFile
  T.writeFile "static/jq.js" jq


secureServer :: Manager -> Server SecureApi
secureServer manager = server manager :<|> serveDirectoryFileServer "static"

gatewayApp :: Manager -> Application
gatewayApp manager = serveWithContext secureApi basicAuthServerContext (secureServer manager)
