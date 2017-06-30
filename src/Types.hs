
-- | Types for the system
module Types
  where

import Network.Socket
import Network.WebSockets (Connection)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.Thyme.Clock as Time
import qualified Control.Concurrent.STM as STM

type Name = T.Text
type Mail = T.Text

data Pass = Pass
  { _pSalt :: BS.ByteString
  , _pHash :: BS.ByteString
  }
  deriving (Show, Eq, Ord) -- , NFData)

newtype Timestamp = Timestamp Time.UTCTime
  deriving (Eq, Ord) -- , NFData)

instance Show Timestamp where
  show _ = show "<time>"

data MsgToUser
  = GotMessage RoomName Message
  | JoinedTo RoomName
  | PartedFrom RoomName
  | UserJoined Name RoomName
  | UserParted Name RoomName
  | UserQuit Name RoomName
  | ErrorMsg ErrorMsg
  | Welcome Name
  deriving (Show, Eq, Ord)

data ErrorMsg
  = Error T.Text
  | InvalidCommand BS.ByteString
  deriving (Show, Eq, Ord)

data Sock
  = NetSock Socket
  | WebSock Connection

data User = User
  { _uName :: !Name
  , _uOutQueue :: !(STM.TQueue MsgToUser)
  , _uRooms :: ![RoomName]
  , _uSocket :: Sock
  }

data Room = Room
  { _rName  :: !RoomName
  , _rTitle :: !T.Text
  , _rUsers :: !(STM.TVar Users)
  --, _rLog   :: [Message]
  }

type Users = M.Map Name (User, Permission)

type RoomName = Name

type ServerState = STM.TVar Server

data Server = Server
  { rooms :: !(M.Map RoomName Room)
  , users :: !(M.Map Name User)
  , nameVar :: STM.TVar Int
  , logger :: STM.TQueue String
  }

newServer :: IO ServerState
newServer = do
  nv <- STM.newTVarIO 0
  q  <- STM.newTQueueIO
  STM.newTVarIO $
    Server
      { rooms = mempty
      , users = mempty
      , nameVar = nv
      , logger = q
      }
 
toLog :: STM.TVar Server -> String -> IO ()
toLog server msg = STM.atomically $ do
  l <- logger <$> STM.readTVar server
  STM.writeTQueue l msg

data Permission
  = Normal
  | Admin
  deriving (Show, Read, Eq, Ord, Enum) -- , NFData)

data Message = Message
  { _mUser :: Name
  , _mText :: T.Text
  }
  deriving (Show, Read, Eq, Ord) -- , NFData)

