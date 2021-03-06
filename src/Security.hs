{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module Security where

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
import           Gateway


type SecureApi = BasicAuth "Gepaste inlog" Player :> GatewayApi
                 :<|> Raw

secureApi :: Proxy SecureApi
secureApi = Proxy

secureServer :: Manager -> Server SecureApi
secureServer manager = server manager :<|> serveDirectoryFileServer "static"

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

gatewayApp :: Manager -> Application
gatewayApp manager = serveWithContext secureApi basicAuthServerContext (secureServer manager)
