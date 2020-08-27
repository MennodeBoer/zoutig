{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module FutureGame where

import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.ByteString            (ByteString)
import           Data.ByteString.Char8      (pack)
import           Data.List
import           Data.Maybe
import           Data.Proxy

import           GHC.Generics
import           Language.Javascript.JQuery
import           Network.HTTP.Client        hiding (Proxy)
import           Network.Wai
import           Network.Wai.Handler.Warp

import qualified Data.Text                  as T hiding (take)
import           Servant.API
import           Servant.API.Empty
import           Servant.Client
import           Servant.Server

import           Debug.Trace

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad

import qualified Data.Map                   as Map

import           Data.Text.Encoding         (decodeUtf8)
import           Network.HTTP.Client        (defaultManagerSettings, newManager)

import           System.Random

import           Config

type FutureApi =
  "state" :> Get '[JSON] GameState
    :<|> "allowed" :> Capture "team" Team :> Get '[JSON] [Int]
    :<|> "move" :> (Capture "team" Team :> Capture "pawn" Int :> Post '[JSON] String
                    :<|> "skip" :> Post '[JSON] String)

pastApi :: Proxy FutureApi
pastApi = Proxy

server :: Manager -> TVar GameState -> Server FutureApi
server manager var =
  getStateServer var
    :<|> getAllowedServer manager var
    :<|> (postMoveServer manager var :<|> postSkipServer var)

getStateServer :: TVar GameState -> Handler GameState
getStateServer = liftIO . readTVarIO

type PostRollApi = QueryParams "options" Int :> Post '[JSON] String

getAllowedServer :: Manager -> TVar GameState -> Team -> Handler [Int]
getAllowedServer manager var team = do
  state <- liftIO $ readTVarIO var
  let pawns = [p | p <- gsPawns state, pTeam p == team]
  let board = getBoard state
  let opt = filter (\p -> Nothing `elem` fmap (\p' -> if p' > nfield
                                                      then Just p
                                                      else fromJust $ Map.lookup (correction team p') board)
                   (take 6 (reverse [1 .. pPosition p - 1])))
                   pawns
  return (fmap pNumber opt)


postMoveServer :: Manager -> TVar GameState -> Team -> Int -> Handler String
postMoveServer manager var team number = do
  state <- liftIO $ readTVarIO var
  let pawns = pawnsByTeam $ gsPawns state
  let fromPosition = fromJust $ Map.lookup number $ fromJust $ Map.lookup team pawns
  let board = getBoard state
  let options = filter (\p -> let pos = fromPosition - p in
                              pos <= nfield
                              && pos > 0
                              && isNothing (fromJust $ Map.lookup (correction team pos) board))
                       [1..6]
  let url = BaseUrl Http "localhost" rollPort ("/roll/generate/" ++ show (length options))
  getRoll <- liftIO $ runClientM (client getApi) (mkClientEnv manager url)
  case getRoll of
    Left e -> microserviceError e
    Right n -> do
      let move = options !! n
      let toPosition = newPosition fromPosition move
      let next = nextTeam Future team
      let outPlay = filter (\p -> pTeam p /= team && pPosition p == 0) $ gsPawns state
      let updatePawns = Map.insert team (Map.insert number toPosition (fromJust $ Map.lookup team pawns)) pawns
      let resurectUrl = BaseUrl Http "localhost" rollPort ("/roll/generate/" ++ show 15)
      getResurrect <- liftIO $ runClientM (client getApi) (mkClientEnv manager url)
      case getResurrect of 
        Left e -> microserviceError e
        Right n' -> if n' >= length outPlay 
                    then do let newState = GameState (pawnMapToList updatePawns) next
                            liftIO $ atomically $ writeTVar var newState        
                            return "Succes!"
                    else do let (Pawn team' number' _) = outPlay !! n'
                            let updateResurrect = Map.insert team' (Map.insert number' (uncorrection team' (correction team fromPosition))
                                                                                       (fromJust $ Map.lookup team' updatePawns)) 
                                                                   updatePawns
                            let newState = GameState (pawnMapToList updateResurrect) next
                            liftIO $ atomically $ writeTVar var newState        
                            return "Succes!"


postSkipServer :: TVar GameState -> Handler String
postSkipServer var = do state <- liftIO $ readTVarIO var
                        let next = nextTeam Future (gsTurn state)
                        liftIO $ atomically $ writeTVar var (GameState (gsPawns state) next)
                        return "Succes!"

uncorrection :: Team -> Position -> Position
uncorrection t p | p > nfield = nfield + 1 + (p - 1) `mod` 3
                 | otherwise = 1 + ((p - 1) - 2 * fromJust (Map.lookup t cors)) `mod` nfield


newPosition :: Int -> Int -> Int
newPosition p move 
  | p - move == 1 = 0
  | otherwise = p - move


futureApp :: Manager -> TVar GameState -> Application
futureApp manager = serve pastApi . server manager
