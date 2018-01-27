module Types where

import Prelude

import Data.Foreign (F, Foreign, ForeignError(JSONError), fail, readArray, readString)
import Data.Foreign.Index ((!))
import Data.Traversable (traverse)

type RoomName = String
type Name = String


data CommandFromUser
  = SendMessage RoomName String
  | Join RoomName
  | Part RoomName
  | Quit

ppCmdFromUser :: CommandFromUser -> String
ppCmdFromUser = case _ of
  SendMessage r s -> "SendMessage " <> show r <> " " <> show s
  Join r -> "Join " <> show r
  Part r -> "Part " <> show r
  Quit -> "Quit"

data MsgToUser
  = GotMessage RoomName Message
  | JoinedTo RoomName (Array Name)
  | PartedFrom RoomName
  | UserJoined Name RoomName
  | UserParted Name RoomName
  | UserQuit RoomName Name
  | ErrorMsg ErrorMsg
  | Welcome Name


ppMsgToUser :: MsgToUser -> String
ppMsgToUser = case _ of
  GotMessage r m -> "GotMessage " <> show r <> " " <> ppMsg m
  JoinedTo r names -> "JoinedTo " <> show r <> " " <> show names
  PartedFrom r -> "PartedFrom " <> show r
  UserJoined n r -> "UserJoined " <> show n <> " " <> show r
  UserParted n r -> "UserParted " <> show n <> " " <> show r
  UserQuit n r -> "UserQuit " <> show n <> " " <> show r
  ErrorMsg err -> "ErrorMsg " <> ppErrorMsg err
  Welcome name -> "Welcome " <> name

readMsgToUser :: Foreign -> F MsgToUser
readMsgToUser value = do
  msgType <- value ! "msg_type" >>= readString
  case msgType of
    "GotMessage" -> GotMessage
      <$> (value ! "room_name" >>= readString)
      <*> (value ! "message" >>= readMessage)
    "JoinedTo" -> JoinedTo
      <$> (value ! "room_name" >>= readString)
      <*> (value ! "names" >>= readArray >>= traverse readString)
    "PartedFrom" -> PartedFrom
      <$> (value ! "room_name" >>= readString)
    "UserJoined" -> UserJoined
      <$> (value ! "name" >>= readString)
      <*> (value ! "room_name" >>= readString)
    "UserParted" -> UserParted
      <$> (value ! "name" >>= readString)
      <*> (value ! "room_name" >>= readString)
    "UserQuit" -> UserQuit
      <$> (value ! "name" >>= readString)
      <*> (value ! "room_name" >>= readString)
    "Welcome" -> Welcome
      <$> (value ! "name" >>= readString)
    "ErrorMsg" -> ErrorMsg
      <$> (value ! "error" >>= readError)
    _ -> fail $ JSONError "Could not read MsgToUser"

data Message = Message Name String

readMessage :: Foreign -> F Message
readMessage value = do
  Message
    <$> (value ! "name" >>= readString)
    <*> (value ! "message" >>= readString)

ppMsg :: Message -> String
ppMsg (Message n s) = "Message " <> show n <> " " <> show s

data ErrorMsg
  = Error String
  | InvalidCommand String
  | ProtocolError String

readError :: Foreign -> F ErrorMsg
readError value = do
  errorType <- value ! "type" >>= readString
  case errorType of
    "Error" -> Error
      <$> (value ! "error" >>= readString)
    "InvalidCommand" -> InvalidCommand
      <$> (value ! "error" >>= readString)
    _ -> fail $ JSONError "Could not read Error"

ppErrorMsg :: ErrorMsg -> String
ppErrorMsg = case _ of
  Error s -> "Error " <> s
  InvalidCommand s -> "InvalidCommand " <> s
  ProtocolError s -> "ProtocolError " <> s
