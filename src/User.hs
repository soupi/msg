{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module User (sendToUser, receiveFromUser, userQuit, joinRoom) where

import Types
import Data.Monoid
import Data.Foldable
import Control.Arrow
import Control.Monad
import Control.Exception
import Network.Socket (close)
import Network.Socket.ByteString
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Control.Concurrent.STM as STM
import qualified Network.Socket.ByteString as Net

data CommandFromUser
  = SendMessage RoomName T.Text
  | Join RoomName
  | Part RoomName
  | Quit
  deriving (Show, Read, Eq, Ord)

sendToUser :: ServerState -> User -> IO ()
sendToUser server user = forever $ do
  msg <- STM.atomically $ STM.readTQueue (_uOutQueue user)
  let bsMsg = BS.pack (show msg) <> "\n"
  case _uSocket user of
    WebSock _ -> error "WebSockets are not implemented yet."
    NetSock s -> Net.sendAll s bsMsg
  toLog server $
    (T.unpack (_uName user) ++ " >> " ++ show msg)

receiveFromUser :: ServerState -> User -> IO ()
receiveFromUser serverState user = do
  msg <- case _uSocket user of
    NetSock s -> recv s 4096
    WebSock _ -> error "WebSockets are not implemented yet."
  case parseCommand msg of
    Nothing -> do
      STM.atomically $
        _uOutQueue user `STM.writeTQueue` ErrorMsg (InvalidCommand msg)
      receiveFromUser serverState user

    Just cmd -> do
      toLog serverState $
        (T.unpack (_uName user) ++ " << " ++ show msg)
      stop <-
        catch
          (const False <$> act user serverState cmd)
          (\(SomeException _) -> pure True)
      unless stop $
        receiveFromUser serverState user
      

parseCommand :: BS.ByteString -> Maybe CommandFromUser
parseCommand = BS.unpack >>> reads >>> \case
  [(cmd,"")] -> pure cmd
  [(cmd,"\r")] -> pure cmd
  [(cmd,"\n")] -> pure cmd
  [(cmd,"\r\n")] -> pure cmd
  _ -> Nothing

act :: User -> ServerState -> CommandFromUser -> IO ()
act user serverState = \case
  SendMessage room msg -> STM.atomically $ do
    server <- STM.readTVar serverState
    mUs <- isUserInRoom server user room
    case mUs of
      InRoom _ us ->
        mapM_ (((`STM.writeTQueue` GotMessage room (Message (_uName user) msg)) . _uOutQueue) . fst) us
      _ ->
        STM.writeTQueue (_uOutQueue user) (ErrorMsg $ Error $ "You are not a member of the room " <> room)

  Join room -> joinRoom room user serverState

  Part room -> STM.atomically $ do
    server <- STM.readTVar serverState
    isUserInRoom server user room >>= \case
      InRoom r us -> do
        mapM_ (((`STM.writeTQueue` UserParted (_uName user) room) . _uOutQueue) . fst) us
        _uOutQueue user `STM.writeTQueue` PartedFrom room
        STM.modifyTVar (_rUsers r) $
          M.delete (_uName user)

      _ ->
        STM.writeTQueue (_uOutQueue user) (ErrorMsg $ Error $ "You are not a member of the room " <> room)

  Quit -> do
    error $ "User " <> T.unpack (_uName user) <> " quit"

userQuit :: STM.TVar Server -> User -> IO ()
userQuit serverState user = do
  STM.atomically $ do
    server <- STM.readTVar serverState

    for_ (rooms server) $ \r -> do
      users <- STM.readTVar (_rUsers r)
      let (userDeleted, users') = deleted (_uName user) users
      STM.writeTVar (_rUsers r) users'
      when userDeleted $
        mapM_ (((`STM.writeTQueue` UserQuit (_uName user) (_rName r)) . _uOutQueue) . fst) users'
        
    STM.writeTVar serverState $
      server
        { users = M.delete (_uName user) (users server)
        }
  case _uSocket user of
    NetSock s -> close s
    WebSock _ -> pure ()

joinRoom :: RoomName -> User -> STM.TVar Server -> IO ()
joinRoom room user serverState = STM.atomically $ do
    server <- STM.readTVar serverState
    isUserInRoom server user room >>= \case
      InRoom _ _ ->
        STM.writeTQueue (_uOutQueue user) (ErrorMsg $ Error $ "You are already a member of the room " <> room)

      NotInRoom r us -> do
        mapM_ (((`STM.writeTQueue` UserJoined (_uName user) room) . _uOutQueue) . fst) us
        _uOutQueue user `STM.writeTQueue` JoinedTo room
        STM.modifyTVar (_rUsers r) $
          M.insert (_uName user) (user, Normal)

      RoomDoesNotExist -> do
        users <- STM.newTVar $ M.fromList [(_uName user, (user, Admin))]
        let newRoom = Room room "" users
        STM.writeTVar serverState (server { rooms = M.insert room newRoom (rooms server) })
        _uOutQueue user `STM.writeTQueue` JoinedTo room


isUserInRoom :: Server -> User -> RoomName -> STM.STM UserInRoom
isUserInRoom s u rn = do
  let rs = rooms s
  case M.lookup rn rs of
    Nothing -> pure RoomDoesNotExist
    Just r -> do
      us <- STM.readTVar (_rUsers r)
      pure $ case M.lookup (_uName u) us of
        Nothing -> NotInRoom r us
        Just _  -> InRoom r us

data UserInRoom
  = InRoom Room Users
  | NotInRoom Room Users
  | RoomDoesNotExist

deleted :: Ord k => k -> M.Map k v -> (Bool, M.Map k v)
deleted = M.alterF (maybe (False, Nothing) ((True,) . pure))
