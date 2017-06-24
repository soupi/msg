{-# LANGUAGE OverloadedStrings #-}

module User where

import Types
import Data.Monoid
import Control.Monad
import Network.Socket (Socket)
import qualified Network.Socket.ByteString as Net
import qualified Data.ByteString.Char8 as BS
import qualified Control.Concurrent.STM as STM
import qualified Data.Map as M
import qualified Control.Monad.Trans.Maybe as MT
import qualified Control.Monad.Trans.Class as MT
import qualified Data.Text as T

data CommandFromUser
  = SendMessage RoomName Message
  | Join RoomName
  | Part RoomName
  deriving (Show, Eq, Ord)

receiveFromUser :: Socket -> Server -> User -> IO ()
receiveFromUser = undefined

parseCommand :: BS.ByteString -> Either BS.ByteString CommandFromUser
parseCommand = undefined

act :: User -> Server -> CommandFromUser -> IO ()
act user server = \case
  SendMessage room msg -> STM.atomically $ do
    rs <- STM.readTVar (rooms server)
    us <- MT.runMaybeT $ do
        r  <- MT.MaybeT . pure $ M.lookup room rs
        us <- MT.lift $ STM.readTVar (_rUsers r)
        _  <- MT.MaybeT . pure $ M.lookup (_uName user) us
        pure us
    maybe
      notInRoom
      (mapM_ ((`STM.writeTQueue` GotMessage room msg) . _uOutQueue) . fmap fst)
      us
    where
      notInRoom =
        STM.writeTQueue (_uOutQueue user) (ErrorMsg $ Error $ "You are not a member of the room " <> room)

  Join room -> STM.atomically $ do
    rs <- STM.readTVar (rooms server)
    case M.lookup room rs of
      Nothing -> do
        users <- STM.newTVar $ M.fromList [(_uName user, (user, Admin))]
        let newRoom = Room room "" users
        STM.writeTVar (rooms server) (M.insert room newRoom rs)
      Just r -> do
        STM.modifyTVar (_rUsers r) $
          (M.insert (_uName user) (user, Normal))
        

  Part room ->
    undefined

sendToUser :: Socket -> User -> IO ()
sendToUser sock user = forever $ do
  msg <- STM.atomically $ STM.readTQueue (_uOutQueue user)
  let bsMsg = BS.pack . show $ msg
  Net.send sock bsMsg
