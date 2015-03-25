{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Data.Foldable
import qualified Data.Set as Set
import qualified Data.Text as T
import Control.Lens
import Control.Concurrent (threadDelay)
import           MVC
import Ohm.SocketIO ( SocketIO, socketIONew, socketIOWaitForConnection, socketIOOpen
                , sioSend, sioSend_, sioSub
                )
import           Ohm.Component
import           Ohm.HTML
import           Ohm.KeyMaster
import           Prelude       hiding (div, filter, id, map, span)
import           Present.Model
import           Present.Types

import qualified Pipes.Concurrent as PC
import           Deck
import           Slide


foreign import javascript unsafe
  "RevealMarkdown.initialize()"
  convertMDSlides :: IO ()

main :: IO ()
main = do
  _ <- initDomDelegator
  km <- initKeyMaster
  key km "r" convertMDSlides
  (keySink, keySource) <- spawn unbounded
  withKeys km keySink [
      ("left", PrevSlide)
    , ("right", NextSlide)
    , ("v", ChangeLayout V)
    , ("h", ChangeLayout H)
    , ("1", ToSection "intro")
    , ("2", ToSection "problem")
    ]
  s <- socketIONew "http://localhost:8000"
  putStrLn "socket"
  socketIOOpen s
  socketIOWaitForConnection s

  modelSink <- runComponent (AppState deck SModel) () slideComponent

  sioSub s "sessions" $ \(Set.toList -> keys) -> do
    liftIO $ putStrLn . T.unpack $ T.unlines keys
    forM_ (keys ^? _head) $ \k ->
      sioSend s "client connect" $ ClientSession (k, "display")

  sioSub s "slide command" $ sendToModel modelSink
  sioSub s "reload md" reloadMD
  forkProcessor () $ for (fromInput keySource) (runProcessor idProcessor)
               >-> (toOutput modelSink)

  where
    reloadMD :: Bool -> IO ()
    reloadMD _ = threadDelay 200000 >> convertMDSlides
    sendToModel evts a = do
      putStrLn $ "slide command " ++ (show a)
      void . atomically $ PC.send evts a

