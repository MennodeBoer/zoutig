{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Roll where

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

import qualified Data.Text                  as T
import           Data.Monoid 

import           Servant.API
import           Servant.API.Empty
import           Servant.Client
import           Servant.Server

import           Data.Attoparsec.Text
import           Debug.Trace

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad

import qualified Data.Map                   as Map

import           Data.Text.Encoding         (decodeUtf8)
import           Network.HTTP.Client        (defaultManagerSettings, newManager)
import           System.Random

import           Config

type RandomApi = "roll" :> (Get '[JSON] Int     
                            :<|> Post '[JSON] String                       
                            :<|> "generate" :> (Capture "length" Int :> Get '[JSON] Int
                                              :<|> Post '[JSON] String))

randomApi :: Proxy RandomApi
randomApi = Proxy

server :: TVar Int -> Server RandomApi
server var = getStoredRollServer var
             :<|> postRollServer var
             :<|> getGenerateRollServer 
             :<|> postForceServer var

getStoredRollServer :: TVar Int -> Handler Int
getStoredRollServer = liftIO . readTVarIO

postRollServer :: TVar Int -> Handler String
postRollServer var = do n <- liftIO $ randomPick 6
                        liftIO $ atomically $ writeTVar var ([1..6] !! n) 
                        return "Succes!"

getGenerateRollServer :: Int -> Handler Int 
getGenerateRollServer = liftIO . randomPick

postForceServer :: TVar Int -> Handler String 
postForceServer var = do liftIO $ atomically $ writeTVar var 6
                         return "Succes!"

randomPick :: Int -> IO Int
randomPick 0 = return 6
randomPick length =
  getStdRandom $ \g -> randomR (0, length - 1) g
                       


rollApp :: TVar Int -> Application 
rollApp = serve randomApi . server
