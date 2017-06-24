
-- | Types for the system
module Types
  where

import Network.Socket (Socket)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
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
  | JoinedFrom RoomName
  | PartedFrom RoomName
  | ErrorMsg ErrorMsg
  deriving (Show, Eq, Ord)

data ErrorMsg
  = Error T.Text
  | InvalidCommand BS.ByteString
  deriving (Show, Eq, Ord)

data User = User
  { _uName :: Name
  , _uOutQueue :: STM.TQueue MsgToUser
  , _uRooms :: [RoomName]
  }

data Room = Room
  { _rName  :: RoomName
  , _rTitle :: T.Text
  , _rUsers :: STM.TVar (M.Map Name (User, Permission))
  --, _rLog   :: [Message]
  }

type RoomName = Name

data Server = Server
  { rooms :: STM.TVar (M.Map RoomName Room)
  , users :: STM.TVar (M.Map Name User)
  }

data Permission
  = Normal
  | Admin
  deriving (Show, Read, Eq, Ord, Enum) -- , NFData)

data Message = Message
  { _mText :: TL.Text
  , _mUser :: Name
  , _mTime :: Timestamp
  }
  deriving (Show, Eq, Ord) -- , NFData)

