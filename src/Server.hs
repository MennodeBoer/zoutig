{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Proxy
import Data.List
import Data.Maybe
import Data.Text as T (Text)
import Data.Text.IO as T (writeFile, readFile)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import GHC.Generics
import Language.Javascript.JQuery
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Data.Text as T
import Servant
import Servant.Server (BasicAuthCheck (BasicAuthCheck),
                       BasicAuthResult( Authorized
                                      , Unauthorized
                                      ),
                       Context ((:.), EmptyContext),
                       err401, err403, errBody, Server,
                       serveWithContext, Handler)

import Debug.Trace
import Servant.JS
import System.Random
import Data.Attoparsec.Text hiding (number)

import Control.Monad
import Control.Concurrent
import qualified Data.Map as Map
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Wai.Handler.Warp
import Servant.Client
import System.Random


type API = "user" :> Get '[JSON] User
           :<|> "winner" :> Get '[JSON] (Maybe Team)
           :<|> "state" :> Get '[JSON] GameState
           :<|> Capture "time" Bool :> "board" :> Get '[JSON] Board
           :<|> "turn" :> Get '[JSON] Bool
           :<|> "throw" :> ("roll" :> QueryParams "options" Int :> PostCreated '[JSON] Int
                            :<|> Get '[JSON] Int
                            :<|> Capture "number" Int :> Get '[JSON] Int
                            :<|> "erase" :> Get '[JSON] ())
           :<|> "move" :> (QueryParams "make" Int :> Put '[JSON] ()
                           :<|> "resurrect" :> Put '[JSON] (Maybe Pawn)
                           :<|> Capture "amount" Int :> "allowed" :> Get '[JSON] [Int])


type SecureAPI = BasicAuth "Gepaste inlog" Player :> API
               :<|> Raw

data User = User
  { username :: Player
  , teamcolor :: Team
  , time :: Bool
  } deriving (Show, Generic)

user :: Player -> Team -> Bool -> User
user = User

instance ToJSON User
instance FromJSON User

data Team = Red | Purple | Blue | Green | Orange
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Team
instance FromJSON Team


data Player = Lukas    | Brian
            | Erik     | Jori
            | Julius   | Michael
            | Marc     | Nijs
            | Richelle | Menno
  deriving (Show, Eq , Ord, Generic)

instance ToJSON Player
instance FromJSON Player

nfield :: Int
nfield = 30

names :: [Player]
names = [Lukas,Brian,Erik,Jori,Julius,Michael,Marc,Nijs,Richelle,Menno]

namesByte :: Map.Map ByteString Player
namesByte = Map.fromList $ [(pack (show p) , p) | p <- names]

teamsByte :: Map.Map Text Team
teamsByte = Map.fromList $ [(T.pack (show t) , t) | t <- [Red,Purple,Blue,Green,Orange]]

teams :: [Team]
teams = [Red,Red,Purple,Purple,Blue,Blue,Green,Green,Orange,Orange]

times :: [Bool]
times = [False,True,False,True,False,True,False,True,False,True]

playerByName :: Map.Map Player User
playerByName = Map.fromList $ foldr (\(name,team,time) us -> (name , User name team time) : us) [] (zip3 names teams times)
  where zip3 (x:xs) (y:ys) (z:zs) = (x,y,z):zip3 xs ys zs
        zip3 _ _ _ = []

nextTeam :: User -> Team -> Team
nextTeam usr team = fromJust $ Map.lookup team $ Map.fromList $ if (time usr) then zip [Orange,Green,Blue,Purple,Red] [Green,Blue,Purple,Red,Orange] else zip [Red,Purple,Blue,Green,Orange] [Purple,Blue,Green,Orange,Red]

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
                                   else return (Authorized u')


basicAuthServerContext :: Context (BasicAuthCheck Player ': '[])
basicAuthServerContext = authCheck :. EmptyContext

api :: Proxy API
api = Proxy

server :: Player -> Server API
server name =
  let
  player = fromJust $ Map.lookup name playerByName
  in
    return player
    :<|> liftIO serverWinner
    :<|> liftIO (getGameState (time player))
    :<|> liftIO . fmap getBoard . getGameState
    :<|> serverTurn player
    :<|> (liftIO . serverRandom
          :<|> liftIO serverThrow
          :<|> liftIO . serverThrowFuture player
          :<|> serverErase)
    :<|> (serverMove player
          :<|> liftIO (serverResurrect player)
          :<|> liftIO . if time player then serverAllowedFuture player else serverAllowed player)

serverWinner :: IO (Maybe Team)
serverWinner = do gamePast <- getGameState False
                  gameFuture <- getGameState True
                  let posPast = Map.map (sort . fmap position . Map.elems) $ pawnsByTeam $ pws gamePast
                  let posFuture = Map.map (sort . fmap position . Map.elems) $ pawnsByTeam $ pws gameFuture
                  let winners = filter (\t -> Map.lookup t posPast == Map.lookup t posFuture) [Red,Purple,Blue,Green,Orange]
                  let winnersPast = filter (\t -> and $ fmap ((<) nfield) (fromJust $ Map.lookup t posPast)) [Red,Purple,Blue,Green,Orange]
                  -- let winnersFuture = filter (\t -> and $ fmap ((==) 0) (fromJust $ Map.lookup t posFuture)) [Red,Purple,Blue,Green,Orange]
                  return (listToMaybe (winners ++ winnersPast))


serverResurrect :: User -> IO (Maybe Pawn)
serverResurrect usr = do game <- getGameState True
                         let pawns = pws game
                         let inPlay = filter (\p -> position p > 0 && position p <= nfield) pawns
                         let nopen = nfield - length inPlay
                         let outPlay = fmap Just $ filter (\p -> team p /= teamcolor usr && position p == 0) pawns
                         let fake = replicate (nopen - length outPlay) Nothing
                         let opt = outPlay ++ fake
                         randomPick opt

serverTurn :: User ->  Handler Bool
serverTurn usr = liftIO (do game <- getGameState (time usr)
                            let t = turn game
                            return (teamcolor usr == t))

serverMove :: User -> [Int] -> Handler ()
serverMove usr [] = liftIO (skipTurn usr)
serverMove usr (move:num:_) = liftIO $ makeMove usr num move
serverMove usr _ = throwError err404

serverAllowed :: User -> Int -> IO [Int]
serverAllowed usr throw = do game <- getGameState False
                             let pawns = Map.elems . fromJust $ Map.lookup (teamcolor usr) $ pawnsByTeam $ pws game
                             let opt = filter (\p -> if throw == 6 then position p <= nfield else (position p > 0 && position p <= nfield)) pawns
                             let ones = filter (\p -> position p == 1) pawns
                             return (if length ones > 0 then fmap number ones else fmap number opt)

serverAllowedFuture :: User -> Int -> IO [Int]
serverAllowedFuture usr _ = do game <- getGameState True
                               let ps = Map.elems $ fromJust $ Map.lookup (teamcolor usr) $ pawnsByTeam $ pws game
                               let board = getBoard game
                               let opt = filter (\p -> Nothing `elem` fmap (\p' -> if p' > nfield then Just p else fromJust $ Map.lookup (correction (teamcolor usr) p') board) (Data.List.take 6 (reverse [1 .. position p - 1]))) ps
                               return (fmap number opt)

randomPick :: [a] -> IO a
randomPick [] = error "whoops"
randomPick options =
  do n <- getStdRandom $ \g ->
                           do let (rx, g')  = randomR (0, length options - 1) g
                                in (options !! rx , g')
     return n

serverRandom :: [Int] -> IO Int
serverRandom [] = return 0
serverRandom options =
  do n <- randomPick options
     contents <- T.readFile "static/Throw.txt"
     when (T.length contents == 0) $ T.writeFile "static/Throw.txt" (T.pack $ show $ n)
     return n


serverThrow :: IO Int
serverThrow = do contents <- T.readFile "static/Throw.txt"
                 return (if T.length contents == 0 then 0 else read (T.unpack contents))

serverThrowFuture :: User -> Int -> IO Int
serverThrowFuture usr n = do game <- getGameState True
                             let p = fromJust $ Map.lookup n $ fromJust $ Map.lookup (teamcolor usr) $ pawnsByTeam $ pws game
                             let board = getBoard game
                             let moves = filter (\m -> position p - m > 0 && position p - m <= nfield && isNothing (fromJust (Map.lookup (correction (teamcolor usr) (position p - m)) board))) [1..6]
                             randomPick moves

serverErase :: Handler ()
serverErase = liftIO $ T.writeFile "static/Throw.txt" ""

secureApi :: Proxy SecureAPI
secureApi = Proxy

secureServer :: Server SecureAPI
secureServer = server :<|> serveDirectoryFileServer "static"

app :: Application
app = serveWithContext secureApi basicAuthServerContext secureServer

startApp :: IO ()
startApp = writeJSFiles >> run 8000 app


apiJS1 :: Text
apiJS1 = jsForAPI api jquery

writeJSFiles :: IO ()
writeJSFiles = do
  T.writeFile "static/api.js" apiJS1
  jq <- Language.Javascript.JQuery.file >>= T.readFile
  T.writeFile "static/jq.js" jq



-- Actuall game:

data Pawn = Pawn {team :: Team
                 , number :: Int
                 , position :: Int}
  deriving (Show, Eq, Generic)

instance ToJSON Pawn
instance FromJSON Pawn

type Position = Int

pawns :: Map.Map Team (Map.Map Int Pawn)
pawns = Map.fromList $ fmap (\t -> (t , Map.fromList $ [(x,Pawn t x 0) | x <- [1,2,3]])) [Red,Purple,Blue,Green,Orange]

printPawns :: User -> Team -> Map.Map Team (Map.Map Int Pawn) -> IO ()
printPawns usr t pws = do let str = unlines $ fmap (\p -> show (team p) ++ " "  ++ show (number p) ++ " " ++ show (position p)) $ concatMap Map.elems (Map.elems pws)
                          if (time usr) then T.writeFile "static/GameStateFuture.txt" (T.pack $ (show t) ++  "\n" ++ str) else T.writeFile "static/GameState.txt" (T.pack $ (show t) ++  "\n" ++ str)


data GameState = GameState {pws :: [Pawn]
                           , turn :: Team}
                 deriving (Show, Generic)

instance ToJSON GameState
instance FromJSON GameState

type Board = Map.Map Position (Maybe Pawn)

emptyBoard :: Board
emptyBoard = Map.fromList $ [(n,Nothing) | n <- [1..54]]

getBoard :: GameState -> Board
getBoard (GameState pws _) = foldr (\p b -> if position p == 0 then b else Map.insert (correction (team p) (position p)) (Just p) b) emptyBoard pws

cors :: Map.Map Team Int
cors = Map.fromList $ zip [Red,Purple,Blue,Green,Orange] [0,3,6,9,12]

correction :: Team -> Position -> Position
correction t p | p > nfield = nfield + fromJust (Map.lookup t cors) + (p - nfield)
               | otherwise = 1 + ((p - 1) + 2 * fromJust (Map.lookup t cors)) `mod` nfield

uncorrection :: Team -> Position -> Position
uncorrection t p | p > nfield = nfield + 1 + (p - 1) `mod` 3
                 | otherwise = 1 + ((p - 1) - 2 * fromJust (Map.lookup t cors)) `mod` nfield


createPawn :: [Text] -> Maybe Pawn
createPawn (t:n:p:xs)  = Map.lookup t teamsByte >>= \t' -> Just $ Pawn t' (read (T.unpack n)) (read (T.unpack p))
createPawn _ = Nothing


getGameState :: Bool -> IO GameState
getGameState time = do str <- if time then T.readFile "static/GameStateFuture.txt" else T.readFile "static/GameState.txt"
                       let ls = T.lines str
                       let t = head ls
                       let pws = fmap (fromJust . createPawn . T.words) $ tail ls
                       let t' = fromJust $ Map.lookup t teamsByte
                       return (GameState pws t')


pawnsByTeam :: [Pawn] -> Map.Map Team (Map.Map Int Pawn)
pawnsByTeam pws = Map.fromList $ [(t , Map.fromList $ [(number p,p) | p <- ps]) | ps <- byTeams, let t = team (head ps)]
  where byTeams = groupBy (\p p' -> team p == team p') pws

makeMove :: User -> Int -> Int -> IO ()
makeMove usr n move = do game <- getGameState (time usr)
                         let t = turn game
                         let board = getBoard $ game
                         let ps = pawnsByTeam $ pws game
                         let p = fromJust $ Map.lookup n $ fromJust $ Map.lookup t ps
                         let movedPawn = if position p == 0
                                         then Pawn (team p) (number p) 1
                                         else Pawn (team p) (number p) (if time usr
                                                                               then newPositionFuture (position p) move
                                                                               else newPosition (position p) move)
                         case fromJust (Map.lookup (correction (team p) (max 1 (position movedPawn))) board) of
                           Nothing -> do let updMove = Map.insert (team p) (Map.insert n movedPawn (fromJust $ Map.lookup t ps)) ps
                                         let updTeam = if position p == 0 then t else nextTeam usr t
                                         res <- serverResurrect usr
                                         case res of
                                          Nothing -> printPawns usr updTeam updMove
                                          Just p' -> do let updRes = if position p <= nfield && time usr
                                                                     then Map.insert (team p') (Map.insert (number p') (Pawn (team p') (number p') (uncorrection (team p') (correction (team p) (position p)))) (fromJust $ Map.lookup (team p') updMove)) updMove
                                                                     else updMove
                                                        printPawns usr updTeam updRes
                           Just p' -> let updPws = Map.insert t (Map.insert n movedPawn (fromJust $ Map.lookup t ps)) ps
                                          updHit = if movedPawn == p'
                                                   then updPws
                                                   else Map.insert (team p') (Map.insert (number p') (Pawn (team p') (number p') 0) (fromJust $ Map.lookup (team p') updPws)) updPws
                                          updTeam = if position p == 0 then t else nextTeam usr t
                                      in
                                        printPawns usr updTeam updHit


skipTurn :: User -> IO ()
skipTurn usr = do game <- getGameState (time usr)
                  let ps = pawnsByTeam $ pws game
                  let t = turn game
                  let updatet = nextTeam usr t
                  printPawns usr updatet ps

newPosition :: Int -> Int -> Int
newPosition p move | p + move > nfield + 3 = nfield + 3 - (p + move - (nfield + 3))
                   | otherwise = p + move

newPositionFuture :: Int -> Int -> Int
newPositionFuture p move | p - move == 1 = 0
                         | otherwise = p - move
