{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Present.State          (ServerState (..), server)

import qualified Control.Concurrent.STM as STM
import qualified Data.Map.Strict        as Map
import qualified Network.EngineIO.Snap  as EIOSnap
import qualified Network.SocketIO       as SocketIO
import qualified Snap.Core              as Snap
import qualified Snap.CORS              as CORS
import qualified Snap.Http.Server       as Snap
import qualified Snap.Util.FileServe    as Snap

import           Paths_revealjs_server  (getDataDir)

main :: IO ()
main = do
  state <- ServerState <$> STM.newTVarIO Map.empty
  socketIoHandler <- SocketIO.initialize EIOSnap.snapAPI (server state)
  dataDir <- getDataDir
  Snap.quickHttpServe $ CORS.applyCORS CORS.defaultOptions $
    Snap.route [ ("/socket.io", socketIoHandler)
               , ("/", Snap.serveDirectory dataDir)
               ]

