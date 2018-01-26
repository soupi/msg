module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Var (($=))
import Control.Monad.IO.Effect (INFINITY)
import Control.Monad.IOSync (runIOSync)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Tuple (Tuple(..))
import Specular.Dom.Builder.Class (dynText, el, text)
import Specular.Dom.Widget (class MonadWidget, runMainWidgetInBody)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.FRP (Dynamic, Event, attachDynWith, changed, fixFRP, foldDyn, holdDyn, mergeEvents, never, newEvent, subscribeEvent_, weaken)
import WebSocket (Connection(Connection), Message(Message), URL(URL), newWebSocket, runMessage, runMessageEvent)

main :: Eff (infinity :: INFINITY) Unit
main = runIOSync $ runMainWidgetInBody mainWidget

mainWidget :: forall m. MonadWidget m => m Unit
mainWidget = connectBtn

data Action
  = Connect
  | Disconnect

connectBtn :: forall m. MonadWidget m => m Unit
connectBtn = do
  connAction <- el "div" do
    fixFRP $ \omega -> do

      let
        flipAction _ = case _ of
          Disconnect -> Connect
          Connect -> Disconnect
      isConnected <- foldDyn flipAction Disconnect omega.connectE
      
      connectE <- buttonOnClick (pure mempty) do
        dynText $ weaken $ flip map isConnected case _ of
          Disconnect -> "Connect"
          Connect -> "Disconnect"
        pure unit

      pure $ Tuple {connectE} (changed isConnected)

  result <- openConn connAction

  sendHelloE <- buttonOnClick (pure mempty) (text "Send Hello")

  flip subscribeEvent_ (attachDynWith Tuple result.sock sendHelloE) \(Tuple soc ev) ->
    case soc of
      Nothing -> pure unit
      Just (Connection socket) -> liftEff $ socket.send (Message "hello")

  el "p" $ dynText <<< weaken <<< map showSockMsg =<< holdDyn SockClose result.msgsEvent
  
  pure unit

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

openConn :: forall m. MonadWidget m => Event Action -> m ({ sock :: Dynamic (Maybe Connection), msgsEvent :: Event SockMsg })
openConn actE = do
  socketE <- newEvent
  sock    <- holdDyn Nothing socketE.event
  msgsE   <- newEvent

  flip subscribeEvent_ (attachDynWith Tuple sock actE) \(Tuple soc ev) ->
    case ev of
      Disconnect ->
        case soc of
          Nothing -> pure unit
          Just (Connection soc) -> liftEff soc.close
        
      Connect -> liftEff do
        Connection socket <- newWebSocket (URL "ws://echo.websocket.org") []

        socket.onopen $= \event -> do
          runIOSync $ do
            socketE.fire (Just $ Connection socket)
            msgsE.fire SockOpen

        socket.onmessage $= \event -> do
          let received = runMessage (runMessageEvent event)
          runIOSync $ msgsE.fire $ SockMsg received

        socket.onclose $= \event -> do
          runIOSync $ do
            socketE.fire Nothing
            msgsE.fire SockClose

  pure { sock, msgsEvent: msgsE.event }
