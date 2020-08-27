{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module PastGame where

import           Config
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Attoparsec.Text
import           Data.ByteString            (ByteString)
import           Data.ByteString.Char8      (pack)
import           Data.List
import qualified Data.Map                   as Map
import           Data.Maybe
import           Data.Proxy
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8)
import           Data.Text.IO               as T (readFile, writeFile)
import           Debug.Trace
import           GHC.Generics
import           Language.Javascript.JQuery
import           Network.HTTP.Client        (defaultManagerSettings, newManager)
import           Network.HTTP.Client        hiding (Proxy)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant.API
import           Servant.API.Empty
import           Servant.Client
import           Servant.Server
import           System.Random

type PastApi =
  "state" :> Get '[JSON] GameState
    :<|> "allowed" :> Capture "team" Team :> Get '[JSON] [Int]
    :<|> "move" :> (Capture "team" Team :> Capture "pawn" Int :> Post '[JSON] String
                    :<|> "skip" :> Post '[JSON] String)

pastApi :: Proxy PastApi
pastApi = Proxy

server :: Manager -> TVar GameState -> Server PastApi
server manager var =
  getStateServer var
    :<|> getAllowedServer manager var
    :<|> (postMoveServer manager var :<|> postSkipServer manager var)

getStateServer :: TVar GameState -> Handler GameState
getStateServer = liftIO . readTVarIO

getAllowedServer :: Manager -> TVar GameState -> Team -> Handler [Int]
getAllowedServer manager var team = do
  state <- liftIO $ readTVarIO var
  let url = BaseUrl Http "localhost" rollPort "/roll"
  let pawns = [p | p <- gsPawns state, pTeam p == team]
  getRoll <- liftIO $ runClientM (client getApi :: ClientM Int) (mkClientEnv manager url)
  case getRoll of
    Left e -> microserviceError e
    Right roll -> do
      let opt = filter (\p -> if roll == 6 then pPosition p <= nfield else pPosition p > 0 && pPosition p <= nfield) pawns
      let ones = filter (\p -> pPosition p == 1) pawns
      return (if null ones then fmap pNumber opt else fmap pNumber ones)

postMoveServer :: Manager -> TVar GameState -> Team -> Int -> Handler String
postMoveServer manager var team number = do
  state <- liftIO $ readTVarIO var
  let url = BaseUrl Http "localhost" rollPort "/roll"
  let board = getBoard state
  let pawns = pawnsByTeam $ gsPawns state
  let fromPosition = fromJust $ Map.lookup number $ fromJust $ Map.lookup team pawns
  let next = if fromPosition == 0 then team else nextTeam Past team
  let moveBoard = Map.insert (correction team fromPosition) Nothing board
  getRoll <- liftIO $ runClientM (client getApi) (mkClientEnv manager url)
  case getRoll of
    Left e -> microserviceError e
    Right move -> do
      let toPosition =
            if fromPosition == 0
              then 1
              else newPosition fromPosition move
      let updatePawns = Map.insert team (Map.insert number toPosition (fromJust $ Map.lookup team pawns)) pawns
      case fromJust $ Map.lookup (correction team toPosition) moveBoard of
        Nothing -> do
          let newState = GameState (pawnMapToList updatePawns) next
          liftIO $ atomically $ writeTVar var newState         
        Just (Pawn team' number' _) -> do
          let updateHit = Map.insert team' (Map.insert number' 0 (fromJust $ Map.lookup team' pawns)) updatePawns
          let newState = GameState (pawnMapToList updateHit) next          
          liftIO $ atomically $ writeTVar var newState        
      reRoll <- liftIO $ runClientM (client postApi) (mkClientEnv manager url)
      either microserviceError return reRoll       
  

postSkipServer :: Manager -> TVar GameState -> Handler String 
postSkipServer manager var = do state <- liftIO $ readTVarIO var
                                let url = BaseUrl Http "localhost" rollPort "/roll"     
                                let next = nextTeam Past (gsTurn state)
                                liftIO $ atomically $ writeTVar var (GameState (gsPawns state) next)
                                reRoll <- liftIO $ runClientM (client postApi :: ClientM Int) (mkClientEnv manager url)
                                return "Succes!"


newPosition :: Int -> Int -> Int
newPosition p move
  | p + move > nfield + 3 = nfield + 3 - (p + move - (nfield + 3))
  | otherwise = p + move


pastApp :: Manager -> TVar GameState -> Application
pastApp manager = serve pastApi . server manager