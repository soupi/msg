{-# LANGUAGE OverloadedStrings #-}

-- | Types for the system
module Msg.Types
  where

import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T
import Data.Aeson hiding (Error)

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

instance ToJSON MsgToUser where
  toJSON = \case
    GotMessage room msg ->
      object
        ["msg_type" .= String "GotMessage"
        , "room_name" .= String room
        , "message" .= toJSON msg
        ]
    JoinedTo room names ->
      object
        ["msg_type" .= String "JoinedTo"
        , "room_name" .= String room
        , "names" .= map String names
        ]
    PartedFrom room ->
      object
        ["msg_type" .= String "PartedFrom"
        , "room_name" .= String room
        ]
    UserJoined name room ->
      object
        ["msg_type" .= String "UserJoined"
        , "name" .= String name
        , "room_name" .= String room
        ]
    UserParted name room ->
      object
        ["msg_type" .= String "UserParted"
        , "name" .= String name
        , "room_name" .= String room
        ]
    UserQuit name room ->
      object
        ["msg_type" .= String "UserQuit"
        , "name" .= String name
        , "room_name" .= String room
        ]
    Welcome name ->
      object
        ["msg_type" .= String "Welcome"
        , "name" .= String name
        ]
    ErrorMsg err ->
      object
        ["msg_type" .= String "ErrorMsg"
        ,"error" .= toJSON err
        ]

instance ToJSON Message where
  toJSON msg =
    object
      [ "name" .= _mUser msg
      , "message" .= _mText msg
      ]

instance ToJSON ErrorMsg where
  toJSON = \case
    Error err ->
      object
        [ "type" .= String "Error"
        , "error" .= String err
        ]

    InvalidCommand err ->
      object
        [ "type" .= String "InvalidCommand"
        , "error" .= T.decodeUtf8 err
        ]
