{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Config where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Proxy
import Data.List
import Data.Maybe
import Data.Text.IO as T (writeFile, readFile)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)

import GHC.Generics
import Language.Javascript.JQuery
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Client hiding (Proxy)

import qualified Data.Text as T
import Servant.API
import Servant.API.Empty
import Servant.Server 
import Servant.Client

import Debug.Trace
import Data.Attoparsec.Text

import Control.Monad
import Control.Monad.Except
import Control.Concurrent

import qualified Data.Map as Map

import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Client (newManager, defaultManagerSettings)

import System.Random

data User = User
  { uName :: Player
  , uTeam :: Team
  , uTime :: Time
  } deriving (Show, Generic)

instance ToJSON User
instance FromJSON User

data Team = Red | Purple | Blue | Green | Orange
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Team
instance FromJSON Team
instance FromHttpApiData Team where
  parseUrlPiece "Red" = Right Red
  parseUrlPiece "Purple" = Right Purple
  parseUrlPiece "Blue" = Right Blue
  parseUrlPiece "Green" = Right Green
  parseUrlPiece "Orange" = Right Orange
  parseUrlPiece _ = Left "no parse"


data Time = Past | Future
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Time
instance FromJSON Time

data Player = Lukas    | Brian
            | Erik     | Jori
            | Julius   | Michael
            | Marc     | Nijs
            | Richelle | Menno
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Player
instance FromJSON Player

data GameState = GameState {gsPawns :: [Pawn]
                           , gsTurn :: Team}
                 deriving (Show, Generic)

instance ToJSON GameState
instance FromJSON GameState

type Board = Map.Map Position (Maybe Pawn)

cors :: Map.Map Team Int
cors = Map.fromList $ zip [Red,Purple,Blue,Green,Orange] [0,3,6,9,12]

correction :: Team -> Position -> Position
correction t p | p > nfield = nfield + fromJust (Map.lookup t cors) + (p - nfield)
               | otherwise = 1 + ((p - 1) + 2 * fromJust (Map.lookup t cors)) `mod` nfield

emptyBoard :: Board
emptyBoard = Map.fromList [(n,Nothing) | n <- [1..nfield + nplayers * 3]]

getBoard :: GameState -> Board
getBoard (GameState pws _) = foldr (\p b -> if pPosition p == 0 then b else Map.insert (correction (pTeam p) (pPosition p)) (Just p) b) emptyBoard pws

data Pawn = Pawn {pTeam :: Team
                 , pNumber :: Int
                 , pPosition :: Position}
  deriving (Show, Eq, Generic)

instance ToJSON Pawn
instance FromJSON Pawn

type Position = Int

nfield :: Int
nfield = 30

nplayers :: Int
nplayers = 5

swapTime :: Time -> Time
swapTime Future = Past
swapTime Past = Future

timeToPort :: Time -> Int
timeToPort Past = 8081
timeToPort Future = 8082

rollPort :: Int
rollPort = 8083

names :: [Player]
names = [Lukas, Brian, Erik, Jori, Julius, Michael, Marc, Nijs, Richelle, Menno]

namesByte :: Map.Map ByteString Player
namesByte = Map.fromList [(pack (show p) , p) | p <- names]

teamsByte :: Map.Map T.Text Team
teamsByte = Map.fromList [(T.pack (show t) , t) | t <- [Red,Purple,Blue,Green,Orange]]

teams :: [Team]
teams = [Red, Red, Purple, Purple, Blue, Blue, Green, Green, Orange, Orange]

times :: [Time]
times = [Past, Future, Past, Future, Past, Future, Past, Future, Past, Future]

userByName :: Map.Map Player User
userByName = Map.fromList $ foldr (\(name,team,time) us -> (name , User name team time) : us) [] (zip3 names teams times)
  where zip3 (x:xs) (y:ys) (z:zs) = (x,y,z):zip3 xs ys zs
        zip3 _ _ _ = []

nextTeam :: Time -> Team -> Team
nextTeam time team = fromJust $ Map.lookup team $ Map.fromList $
      if time == Future then zip [Orange,Green,Blue,Purple,Red] [Green,Blue,Purple,Red,Orange]
                      else zip [Red,Purple,Blue,Green,Orange] [Purple,Blue,Green,Orange,Red]

pawnsByTeam :: [Pawn] -> Map.Map Team (Map.Map Int Position)
pawnsByTeam pws = Map.fromList [(t , Map.fromList [(pNumber p, pPosition p) | p <- ps]) | ps <- byTeams, let t = pTeam (head ps)]
  where byTeams = groupBy (\p p' -> pTeam p == pTeam p') pws

pawnMapToList :: Map.Map Team (Map.Map Int Position) -> [Pawn]
pawnMapToList map = concat $ (\(t, m) -> uncurry (Pawn t) <$> Map.toList m) <$> Map.toList map 

microserviceError :: ClientError -> Handler a
microserviceError e = do liftIO (putStrLn ("Got internal-api error: " ++ show e))
                         throwError $ err500 {errBody = "CyberInternal MicroServer MicroError"}

type GetApi a = Get '[JSON] a

getApi :: forall a. Proxy (GetApi a)
getApi = Proxy

type PostApi a = Post '[JSON] a

postApi :: Proxy (PostApi a)
postApi = Proxy

pastBeginState :: GameState
pastBeginState = GameState (concat ((\t -> [Pawn t n 0 | n <- [1,2,3]]) <$> [Red,Purple,Blue,Green,Orange])) Red
                    
futureBeginState :: GameState
futureBeginState = GameState (concat ((\t -> [Pawn t n (30 + n) | n <- [1,2,3]]) <$> [Red,Purple,Blue,Green,Orange])) Orange