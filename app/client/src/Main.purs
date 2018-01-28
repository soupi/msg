module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Var (($=))
import Control.Monad.Except (runExcept)
import Control.Monad.IO.Effect (INFINITY)
import Control.Monad.IOSync (IOSync, runIOSync)
import Data.Array (delete, sort, uncons, (:))
import Data.Either (Either(..))
import Data.Foldable (fold, foldl, length)
import Data.Foreign (renderForeignError)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Tuple (Tuple(..), snd)
import JSON (foreignValue)
import Specular.Dom.Builder.Class (dynText, el, elAttr, text)
import Specular.Dom.Node.Class ((:=))
import Specular.Dom.Widget (class MonadWidget, runMainWidgetInBody)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.Dom.Widgets.Input (textInput, textInputValue, textInputValueEventOnEnter)
import Specular.FRP (Dynamic, Event, attachDynWith, changed, dynamic, fixFRP, foldDyn, holdDyn, leftmost, mergeEvents, never, newEvent, subscribeEvent_, tagDyn, weaken)
import Types (CommandFromUser(..), ErrorMsg(..), Message(..), MsgToUser(..), ppCmdFromUser, ppErrorMsg, ppMsgToUser, readMsgToUser)
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
  el "div" do
    elAttr "div" ("style" := "display: flex; height: 80%;") do
      messagesWidget state
      nickListWidget state
    void $ inputTextBar state.status.dyn

  el "p" $ dynText <<< weaken <<< map showSockMsg =<< holdDyn SockClose state.msgs.event

messagesWidget :: forall m. MonadWidget m => State -> m Unit
messagesWidget state = do
  messages <- foldDyn ($) (pure unit) $ flip map state.msgs.event case _ of
    SockMsg msg' -> case msg' of
      Welcome name ->
        \msgs -> do
          msgs
          el "li" $ text $ "*** Welcome! You are now known as: " <> name <> "."
      GotMessage _ (Message name msg) ->
        \msgs -> do
          msgs
          el "li" $ text $ "<" <> name <> "> " <> msg 
      JoinedTo room users ->
        \msgs -> do
          msgs
          el "li" $ text $ "* You joined to #" <> room <> ". "
            <> if length users == 0
                  then "You are the first one here."
                  else "Say hello to: " <> commas users
      PartedFrom room ->
        \msgs -> do
          msgs
          el "li" $ text $ "* You parted from #" <> room <> "."
      UserJoined user room ->
        \msgs -> do
          msgs
          el "li" $ text $ "* " <> user <> " joined to #" <> room <> "."
      UserParted user room ->
        \msgs -> do
          msgs
          el "li" $ text $ "* " <> user <> " parted from #" <> room <> "."
      UserQuit user _ ->
        \msgs -> do
          msgs
          el "li" $ text $ "* " <> user <> " quit."
      ErrorMsg err ->
        \msgs -> do
          msgs
          el "li" $ text $ "*** Error: " <> ppErrorMsg err
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
  | SockMsg MsgToUser

showSockMsg :: SockMsg -> String
showSockMsg = case _ of
  SockOpen -> "SockOpen"
  SockClose -> "SockClose"
  SockMsg str -> "SockMsg " <> ppMsgToUser str

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
        Connection socket <- newWebSocket (URL "ws://localhost:8888") []

        socket.onopen $= \event -> do
          runIOSync $ do
            state.status.fire (Connected $ Connection socket)
            state.msgs.fire SockOpen

        socket.onmessage $= \event -> do
          let received = runMessage (runMessageEvent event)
          case runExcept $ readMsgToUser =<< foreignValue received of
            Right msg ->
              runIOSync $ state.msgs.fire $ SockMsg msg
            Left err ->
              runIOSync
                <<< state.msgs.fire
                <<< SockMsg
                <<< ErrorMsg
                <<< ProtocolError
                <<< fold
                <<< map renderForeignError
                  $ err

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
      , attributes: map (_ <> "style" := "width: 90%") enableBtn
      , setValue: "" <$ omega.setE
      }

    setKeyE <- buttonOnClick ((_ <> "style" := "width: 8%") <$> enableBtn) $ text "Send"

    setEnterE <- textInputValueEventOnEnter txt
    let setE = leftmost [setKeyE, unit <$ setEnterE]

    pure (Tuple {setE} $ tagDyn (textInputValue txt) setE)

  flip subscribeEvent_ (attachDynWith Tuple status txtE) \(Tuple state msg) ->
    case state of
      Connected (Connection socket) -> liftEff $ socket.send (WS.Message $ ppCmdFromUser $ SendMessage "Hall" msg)
      _ -> pure unit

  pure txtE


nickListWidget :: forall m. MonadWidget m => State -> m Unit
nickListWidget state = do
  nicks <- foldDyn ($) (Tuple "" []) $ flip map state.msgs.event case _ of
    SockMsg msg' -> case msg' of
      Welcome user ->
        const $ Tuple user []

      JoinedTo room users ->
        \(Tuple me _) -> Tuple me $ sort (me : users)

      PartedFrom room ->
        \(Tuple me _) -> Tuple me []

      UserJoined user room ->
        \(Tuple me users) -> Tuple me $ sort (user : users)

      UserParted user room ->
        \(Tuple me users) -> Tuple me $ delete user users

      UserQuit user _ ->
        \(Tuple me users) -> Tuple me $ delete user users

      _ -> id

    SockClose ->
      const $ Tuple "" []
    _ -> id

  let
    nickList =
      map
        ( foldl (*>) (pure unit)
          <<< map (el "li" <<< text)
          <<< snd
        ) nicks

  void $ elAttr "ul" ("id" := "nickList" <> "style" := nickListStyle) $ dynamic nickList



commas :: Array String -> String
commas words = case uncons words of
  Nothing -> ""
  Just { head, tail } ->
    foldl (\rest next -> rest <> ", " <> next) head tail

messagesStyle :: String
messagesStyle = """
list-style-type: none;
width: 70%;
min-width: 500px;
overflow: auto;
border: 1px solid #eee;
padding-top: 10px;
padding-left: 10px;
/* display: flex; flex-direction: column-reverse; doesn't work for firefox */
"""

nickListStyle :: String
nickListStyle = """
list-style-type: none;
width: 25%;
min-width: 100px;
overflow: auto;
border: 1px solid #eee;
padding-top: 10px;
padding-left: 10px;
"""
