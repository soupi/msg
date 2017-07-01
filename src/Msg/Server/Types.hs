
-- | Types for the server
module Msg.Server.Types
  ( Sock(..)
  , User(..)
  , Room(..)
  , Server(..)
  , Users
  , ServerState
  , toLog
  , module Export
  )
  where

import Msg.Types as Export

import Network.Socket
import Network.WebSockets (Connection)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Control.Concurrent.STM as STM

data Sock
  = NetSock Socket
  | WebSock Connection

data User = User
  { _uName :: !Name
  , _uOutQueue :: !(STM.TQueue MsgToUser)
  , _uRooms :: ![RoomName]
  , _uSocket :: Sock
  }

type Users = M.Map Name (User, Permission)

data Room = Room
  { _rName  :: !RoomName
  , _rTitle :: !T.Text
  , _rUsers :: !(STM.TVar Users)
  --, _rLog   :: [Message]
  }

data Server = Server
  { rooms :: !(M.Map RoomName Room)
  , users :: !(M.Map Name User)
  , nameVar :: STM.TVar Int
  , logger :: STM.TQueue String
  }

type ServerState = STM.TVar Server

toLog :: STM.TVar Server -> String -> IO ()
toLog server msg = STM.atomically $ do
  l <- logger <$> STM.readTVar server
  STM.writeTQueue l msg
