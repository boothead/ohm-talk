{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Lens
import Control.Monad.STM
import Control.Monad.Trans.Reader
import Data.Aeson (ToJSON)
import qualified Data.Set as S
import Pipes
import qualified Pipes.Concurrent as PC
--import Prelude hiding ((.))
import Control.Applicative
import Data.Foldable (traverse_)
import Data.Monoid ((<>))
import Ohm.Component
import Ohm.HTML
import Ohm.SocketIO ( SocketIO, socketIONew, socketIOWaitForConnection, socketIOOpen
                , sioSend, sioSend_, sioSub
                )
import Slide
import Deck
import Present.Model
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

renderPresenter :: Renderer SlideCommand (ClientAppState () ())
renderPresenter = undefined


--------------------------------------------------------------------------------
-- Processor

slideProcessor :: (MonadIO m) => Processor m SlideCommand (SlideEvent edom)
slideProcessor = Processor $ \cmd -> do
  liftIO $ print cmd
  case cmd of
    CPrevSlide -> yield PrevSlide
    CNextSlide -> yield NextSlide
    CToSection s -> yield $ ToSection s
    _ -> return ()

-- slidePresenter :: Component Env (SlideEvent ())
--                                (ClientAppState () ())
--                                (Command ())
slidePresenter = undefined


--------------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  s <- socketIONew "http://localhost:8000"
  putStrLn "socket"
  socketIOOpen s
  socketIOWaitForConnection s
  modelEvents <- runComponent (AppState deck ()) (Env s) slidePresenter
  sioSend s "controller connect" $ CreateSession  ("LHUG", _ deck)
  -- sioSub s "new message"         $ sendToModel modelEvents (Chat . NewChatMessage)               
  -- sioSub s "user joined"         $ sendToModel modelEvents (Chat . NewUser)                    
  -- sioSub s "user left"           $ sendToModel modelEvents (Chat . UserLeft)                   
  -- --sioSub s "login"             $ sendToModel modelEvents (Chat . NewUser)                  
  -- -- sioSub s "typing"              $ sendToModel modelEvents (Chat . SomeoneTyping)              
  -- -- sioSub s "stop typing"         $ sendToModel modelEvents (Chat . StopTyping)                 
  -- sioSub s "currently connected" $ sendToModel modelEvents (Chat . CurrentlyConnected) 
  -- sioSub s "currently typing"    $ sendToModel modelEvents (Chat . CurrentlyTyping) 
  -- sioSub s "state"               $ sendToModel modelEvents (Chat . LoadState)
  where
    sendToModel evts f = void . atomically . PC.send evts . f
