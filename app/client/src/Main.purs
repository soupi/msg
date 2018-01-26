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
import Specular.Dom.Builder.Class (dynText, el, text)
import Specular.Dom.Node.Class ((:=))
import Specular.Dom.Widget (class MonadWidget, runMainWidgetInBody)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.FRP (Dynamic, Event, attachDynWith, changed, fixFRP, foldDyn, holdDyn, mergeEvents, never, newEvent, subscribeEvent_, weaken)
import WebSocket (Connection(Connection), Message(Message), URL(URL), newWebSocket, runMessage, runMessageEvent)

main :: Eff (infinity :: INFINITY) Unit
main = runIOSync $ runMainWidgetInBody mainWidget

mainWidget :: forall m. MonadWidget m => m Unit
mainWidget = do
  statusE <- newEvent
  status  <- holdDyn Disconnected statusE.event
  msgsE   <- newEvent

  connectBtn
    { status:
      { dyn: status
      , fire: statusE.fire
      }
    , msgs: msgsE
    }

data Action
  = Connect
  | Disconnect

connectBtn :: forall m. MonadWidget m => State -> m Unit
connectBtn state = do
  connAction <- el "div" do
    fixFRP $ connectButton $ state.status

  result <- openConn state connAction

  sendHelloE <- buttonOnClick (pure mempty) (text "Send Hello")

  flip subscribeEvent_ (attachDynWith Tuple state.status.dyn sendHelloE) \(Tuple soc ev) ->
    case soc of
      Connected (Connection socket) -> liftEff $ socket.send (Message "hello")
      _ -> pure unit

  el "p" $ dynText <<< weaken <<< map showSockMsg =<< holdDyn SockClose state.msgs.event

  pure unit

connectButton :: forall m. MonadWidget m
  => { dyn :: Dynamic Status, fire :: Status -> IOSync Unit }
  -> { connectE :: Event Unit }
  -> m (Tuple { connectE :: Event Unit } (Event Action))
connectButton status omega = do

      let
        flipAction _ = case _ of
          Disconnect -> Connect
          Connect -> Disconnect

      action <- foldDyn flipAction Disconnect omega.connectE

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
  flip subscribeEvent_ (attachDynWith Tuple state.status.dyn actE) \(Tuple soc ev) ->
    case ev of
      Disconnect ->
        case soc of
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


