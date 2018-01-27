module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Var (($=))
import Control.Monad.IO.Effect (INFINITY)
import Control.Monad.IOSync (IOSync, runIOSync)
import Data.Foldable (foldl)
import Data.Monoid (mempty)
import Data.Tuple (Tuple(..))
import Specular.Dom.Builder.Class (dynText, el, elAttr, text)
import Specular.Dom.Node.Class ((:=))
import Specular.Dom.Widget (class MonadWidget, runMainWidgetInBody)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.Dom.Widgets.Input (textInput, textInputValue, textInputValueEventOnEnter)
import Specular.FRP (Dynamic, Event, attachDynWith, changed, dynamic, fixFRP, foldDyn, holdDyn, leftmost, mergeEvents, never, newEvent, subscribeEvent_, tagDyn, weaken)
import WebSocket (Connection(Connection), URL(URL), newWebSocket, runMessage, runMessageEvent)
import WebSocket (Message(Message)) as WS

foreign import scrollMessages :: forall e. Eff (infinity :: INFINITY | e) Unit

main :: Eff (infinity :: INFINITY) Unit
main = runIOSync $ runMainWidgetInBody do
  statusE <- newEvent
  status  <- holdDyn Disconnected statusE.event
  msgsE   <- newEvent

  let
    state =
      { status:
        { dyn: status
        , fire: statusE.fire
        }
      , msgs: msgsE
      }
  mainWidget state

mainWidget :: forall m. MonadWidget m => State -> m Unit
mainWidget state = do
  connectBtn state

  inputE <- inputTextBar state.status.dyn
  flip subscribeEvent_ (attachDynWith Tuple state.status.dyn inputE) \(Tuple state msg) ->
    case state of
      Connected (Connection socket) -> liftEff $ socket.send (WS.Message $ ppCmdFromUser $ SendMessage "Hall" msg)
      _ -> pure unit

  messagesWidget state

  el "p" $ dynText <<< weaken <<< map showSockMsg =<< holdDyn SockClose state.msgs.event

messagesWidget :: forall m. MonadWidget m => State -> m Unit
messagesWidget state = do
  messages <- foldDyn ($) (pure unit) $ flip map state.msgs.event case _ of
    SockMsg msg -> \msgs -> do
      msgs
      el "li" (text msg)
    SockOpen -> const $ pure unit
    _ -> id

  _ <- elAttr "ul" ("id" := "messages" <> "style" := messagesStyle) $ dynamic messages

  flip subscribeEvent_ state.msgs.event \msg ->
    case msg of
      SockMsg _ -> liftEff do
        scrollMessages
      _ -> pure unit

data Action
  = Connect
  | Disconnect

connectBtn :: forall m. MonadWidget m => State -> m Unit
connectBtn state = do
  connAction <- el "div" do
    fixFRP $ connectButton $ state.status

  result <- openConn state connAction

  pure unit

connectButton :: forall m. MonadWidget m
  => { dyn :: Dynamic Status, fire :: Status -> IOSync Unit }
  -> { connectE :: Event Unit }
  -> m (Tuple { connectE :: Event Unit } (Event Action))
connectButton status omega = do

      let
        chooseAction st _ = case st of
          Disconnected -> Connect
          Connected _  -> Disconnect
          _  -> Connect

      action <- holdDyn Disconnect
        $ attachDynWith chooseAction status.dyn omega.connectE

      let
        enableBtn status = case status of
          Disconnected -> mempty
          Connected _ -> mempty
          WaitClose _ -> "disabled" := show true
          WaitOpen -> "disabled" := show true

      connectE <- buttonOnClick (weaken $ map enableBtn status.dyn) do
        dynText $ weaken $ flip map status.dyn case _ of
          Disconnected -> "Connect"
          Connected _ -> "Disconnect"
          WaitClose _ -> "Wait..."
          WaitOpen -> "Wait..."
        pure unit

      flip subscribeEvent_ (attachDynWith Tuple status.dyn connectE) \(Tuple stat _) ->
        case stat of
          Disconnected -> status.fire WaitOpen
          _ -> pure unit

      pure $ Tuple {connectE} (changed action)


compose2 :: forall a b c. (b -> c) -> (a -> a -> b) -> a -> a -> c
compose2 f g x y = f (g x y)

foldlEvents :: forall a. (a -> a -> a) -> Array (Event a) -> Event a
foldlEvents f = foldl (mergeEvents pure pure (compose2 pure f)) never

data SockMsg
  = SockOpen
  | SockClose
  | SockMsg String

showSockMsg :: SockMsg -> String
showSockMsg = case _ of
  SockOpen -> "SockOpen"
  SockClose -> "SockClose"
  SockMsg str -> "SockMsg " <> str

data Status
  = Connected Connection
  | Disconnected
  | WaitClose Connection
  | WaitOpen

type State =
  { status :: { dyn :: Dynamic Status, fire :: Status -> IOSync Unit }
  , msgs :: { event :: Event SockMsg, fire :: SockMsg -> IOSync Unit }
  }

openConn :: forall m. MonadWidget m
  => State
  -> Event Action
  -> m Unit
openConn state actE = do
  flip subscribeEvent_ (attachDynWith Tuple state.status.dyn actE) \(Tuple st ev) ->
    case ev of
      Disconnect ->
        case st of
          Connected (Connection soc) -> do
            state.status.fire (WaitClose $ Connection soc)
            liftEff soc.close
          _ -> pure unit

      Connect -> liftEff do
        Connection socket <- newWebSocket (URL "wss://echo.websocket.org") []

        socket.onopen $= \event -> do
          runIOSync $ do
            state.status.fire (Connected $ Connection socket)
            state.msgs.fire SockOpen

        socket.onmessage $= \event -> do
          let received = runMessage (runMessageEvent event)
          runIOSync $ state.msgs.fire $ SockMsg received

        socket.onclose $= \event -> do
          runIOSync $ do
            state.status.fire Disconnected
            state.msgs.fire SockClose

  pure unit

-- | An input text bar
--
inputTextBar :: forall m. MonadWidget m => Dynamic Status -> m (Event String)
inputTextBar status = el "div" $ do
  txtE <- fixFRP $ \omega -> do
    let
      enableBtn = weaken $ flip map status case _ of
        Connected _ -> mempty
        _ -> "disabled" := show true

    txt <- textInput
      { initialValue: ""
      , attributes: enableBtn
      , setValue: "" <$ omega.setE
      }

    setKeyE <- buttonOnClick enableBtn $ text "Send"

    setEnterE <- textInputValueEventOnEnter txt
    let setE = leftmost [setKeyE, unit <$ setEnterE]

    pure (Tuple {setE} $ tagDyn (textInputValue txt) setE)

  pure txtE

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
  | UserQuit Name RoomName
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


data Message = Message Name String

ppMsg :: Message -> String
ppMsg (Message n s) = "Message " <> show n <> " " <> show s

data ErrorMsg
  = Error String
  | InvalidCommand String

ppErrorMsg :: ErrorMsg -> String
ppErrorMsg = case _ of
  Error s -> "Error " <> s
  InvalidCommand s -> "InvalidCommand " <> s

messagesStyle :: String
messagesStyle = """
list-style-type: none;
height: 50%;
width: 80%;
min-width: 500px;
overflow: auto;
border: 1px solid #eee;
// display: flex; flex-direction: column-reverse; // doesn't work for firefox
"""
