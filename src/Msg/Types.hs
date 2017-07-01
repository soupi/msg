
-- | Types for the system
module Msg.Types
  where

import qualified Data.Text as T
import qualified Data.ByteString as BS

type Name = T.Text

data CommandFromUser
  = SendMessage RoomName T.Text
  | Join RoomName
  | Part RoomName
  | Quit
  deriving (Show, Read, Eq, Ord)

data MsgToUser
  = GotMessage RoomName Message
  | JoinedTo RoomName [Name]
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

type RoomName = Name

data Permission
  = Normal
  | Admin
  deriving (Show, Read, Eq, Ord, Enum) -- , NFData)

data Message = Message
  { _mUser :: Name
  , _mText :: T.Text
  }
  deriving (Show, Read, Eq, Ord) -- , NFData)

