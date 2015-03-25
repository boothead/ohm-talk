{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Lens
import Control.Monad.STM
import Control.Monad.Trans.Reader
import Data.Aeson (ToJSON)
import qualified Data.Set as Set
import Pipes
import qualified Pipes.Concurrent as PC
--import Prelude hiding ((.))
import Control.Applicative
import qualified Data.Text as T
import Ohm.Component
import Ohm.HTML
import Ohm.SocketIO ( SocketIO, socketIONew, socketIOWaitForConnection, socketIOOpen
                , sioSend, sioSend_, sioSub
                )
import VirtualDom.Prim
import qualified VirtualDom.HTML.Attributes as A
import Slide
import Deck
import Present.Model
import Present.Stack
import Present.Types

data Env = Env {
    ws :: SocketIO
  }

type ProcessorMonad = ReaderT Env IO

logMessage :: (Show a, MonadIO m) => String -> a -> m ()
logMessage msg a = liftIO . putStrLn $ msg ++ ": " ++ (show a)

wsEmit :: (ToJSON a) => String -> a -> ProcessorMonad ()
wsEmit chan msg = do
  sio <- ws <$> ask
  liftIO $ sioSend sio chan msg

wsEmit_ :: String -> ProcessorMonad ()
wsEmit_ chan = do
  sio <- ws <$> ask
  liftIO $ sioSend_ sio chan


--------------------------------------------------------------------------------
-- Render

renderPresenter :: Renderer SlideCommand (ClientAppState SModel Edom)
renderPresenter (DOMEvent chan) (AppState ss _)=
  container [
      row [with div_ (A.classes .= ["btn-group"])
            (btn . mkToSection <$> workspaceTags ss)
          ]
    , row (col6 . (\a -> [a]) . btn
             <$> ([("prev", CPrevSlide), ("next", CNextSlide)] :: [(T.Text, SlideCommand)]))
    ]
  where
    mkToSection sname = (sname, CToSection sname)
    btn (txt, cmd) = mkButton sendMessage (text txt) ["btn-primary", "btn-large"]
      where
        sendMessage = const $ chan cmd

--------------------------------------------------------------------------------
-- Processor

presenterProcessor :: Processor ProcessorMonad SlideCommand (SlideEvent Edom)
presenterProcessor = Processor $ \cmd -> do
  liftIO $ print cmd
  lift $ wsEmit "controller command" cmd
 
slidePresenter :: Component Env (SlideEvent Edom)
                               (ClientAppState SModel Edom)
                                SlideCommand
slidePresenter = Component slideModel renderPresenter p
  where
    p = presenterProcessor


--------------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  _ <- initDomDelegator
  s <- socketIONew "http://192.168.1.225:8000"
  putStrLn "socket"
  socketIOOpen s
  socketIOWaitForConnection s
  modelEvents <- runComponent (AppState deck SModel) (Env s) slidePresenter
  sioSub s "sessions" $ \keys ->
    liftIO $ putStrLn . T.unpack . T.unlines $ Set.toList keys
  sioSend s "controller connect" $ CreateSession "LHUG"
  sioSub s "slide command" $ sendToModel modelEvents
  where
    sendToModel evts a = do
      putStrLn $ "slide command" ++ (show a)
      void . atomically $ PC.send evts a
