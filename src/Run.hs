{-# LANGUAGE OverloadedStrings #-}

module Run where

import Types
import User

import Control.Exception
import Data.Monoid
import Control.Monad
import Network.Socket
import qualified Network.WebSockets as WS
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.Async as Async
import Control.Concurrent (forkFinally)

run :: IO ()
run = do
  server <- newServer
  Async.race_
    (runLogger server)
    $ Async.race_
        (netSocketServer server)
        (webSocketServer server)

webSocketServer :: ServerState -> IO ()
webSocketServer server = do
  toLog server "Waiting for web connections..."
  void $ WS.runServer "0.0.0.0" 8888 $ \pending -> do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 120
    listener (WebSock conn) server

netSocketServer :: ServerState -> IO ()
netSocketServer server = do
  addrInfos <-
    getAddrInfo
    (Just defaultHints{addrFlags = [AI_PASSIVE]})
    Nothing
    (Just "7777")
  let serverAddr = head addrInfos
  bracket
    (socket (addrFamily serverAddr) Stream defaultProtocol)
    (const (toLog server "\nClosed") <=< close) $ \sock -> do
      setSocketOption sock ReuseAddr 1
      bind sock (addrAddress serverAddr)
      listen sock 1
      toLog server "Waiting for connections..."
      void $ forever $ do
        (soc, _) <- accept sock
        forkFinally
          (listener (NetSock soc) server)
          (const $ close soc)

listener :: Sock -> ServerState -> IO ()
listener sock server = do
  bracket
    (STM.atomically $ do
      nvTVar <- nameVar <$> STM.readTVar server
      nv <- STM.readTVar nvTVar
      STM.modifyTVar nvTVar (+1)
      oq <- STM.newTQueue
      let
        name = "Guest" <> T.pack (show nv)
        user = User
          { _uName = name
          , _uOutQueue = oq
          , _uRooms = mempty
          , _uSocket = sock
          }
      oq `STM.writeTQueue` Welcome name
      STM.modifyTVar server $ \s ->
        s { users = M.insert name user $ users s }
      pure user
    )
    (const (toLog server "\nClosed User") <=< userQuit server) $ \user -> do
      joinRoom "Hall" user server
      Async.race_
        (sendToUser server user)
        (receiveFromUser server user)

runLogger :: STM.TVar Server -> IO ()
runLogger server = forever $ do
  msg <- STM.atomically $ do
    l <- logger <$> STM.readTVar server
    STM.readTQueue l
  putStrLn msg
