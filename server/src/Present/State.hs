{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}


module Present.State (
    server
  , ServerState (..)
  ) where

import           Prelude                    hiding (mapM_)

import           Control.Applicative
import qualified Control.Concurrent.STM     as STM
import           Control.Lens
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.State.Strict (StateT)
import           Control.Monad.Trans.Reader (ReaderT)
-- import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.Foldable              (forM_, mapM_)
import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set
-- import           Data.Text                  (Text)
-- import           Data.Typeable
-- import           GHC.Generics
import qualified Network.SocketIO           as SocketIO
import           Present.Model
import           Present.Stack
import           Present.Types
import qualified Snap.Core                  as Snap


--------------------------------------------------------------------------------
data ServerState = ServerState {
   _slideSessions :: STM.TVar SessionMap
 --, _slideLocation :: STM.TVar Location
 }

makeLenses ''ServerState

logMsg :: Show a => String -> a -> IO ()
logMsg msg a = putStrLn (msg ++ ": " ++ show a)

getState :: MonadIO m => ServerState -> m SessionMap
getState state = liftIO $ STM.atomically $ do
  STM.readTVar (state ^. slideSessions)
  --l <- STM.readTVar (state ^. slideLocation)
  -- m <- STM.readTVar (ssMessages state)
  -- return (s ,l)

-- withSession :: (MonadIO m) => ServerState -> SessionName -> (ConnectedState -> m a) -> m ()
-- withSession state sn m = do
--     s <- getState state
--     mapM_ m (s ^? _1.ix sn)

server :: ServerState -> StateT SocketIO.RoutingTable (ReaderT SocketIO.Socket Snap.Snap) ()
server state = do
  sessionNameMVar <- liftIO STM.newEmptyTMVarIO
  userNameMVar <- liftIO STM.newEmptyTMVarIO

  -- let forUserName m = liftIO (STM.atomically (STM.tryReadTMVar userNameMVar)) >>= mapM_ m
  let forSessionName m = liftIO (STM.atomically (STM.tryReadTMVar sessionNameMVar)) >>= mapM_ m
  -- let forCurrentSession m = forSessionName $ \sn -> withSession state sn m

  SocketIO.on "controller connect" $ \(CreateSession sn) -> do
    liftIO $ print ("CONTROLLER CONNECT", sn)
    keys <- liftIO $
      STM.atomically $ do
        STM.modifyTVar' (state ^. slideSessions) $ Map.insertWith const sn (UsersConnected Set.empty)
        STM.putTMVar sessionNameMVar sn
        STM.readTVar (state ^. slideSessions)
    liftIO $ putStrLn "BCASTING"
    SocketIO.broadcast "sessions" keys
    liftIO $ putStrLn "BCAST DONE"
    
  SocketIO.on "controller command" $ \cc -> do
    liftIO $ print ("CONTROLLER COOMMAND", cc)
    let e = cmdToEvent cc
    -- liftIO $ STM.atomically $ do
    --   sn <- STM.tryReadTMVar sessionNameMVar
    --   forM_ ((,) <$> sn <*> f)$ \(sesName, op') ->
    --     STM.modifyTVar' (state ^. slideSessions) $ \sState -> sState & ix sesName %~ op'
    mapM_ (SocketIO.broadcast "slide command") e
    case cc of
      (CToSection _) -> SocketIO.broadcast "reload md" True
      _ -> return ()

  SocketIO.on "client connect" $ \(ClientSession (sn, cname)) -> do
    liftIO $ STM.atomically $ do
      STM.putTMVar sessionNameMVar sn
      STM.putTMVar sessionNameMVar cname
      STM.modifyTVar' (state ^. slideSessions) $ \sState ->
        sState & ix sn . _Wrapped %~ Set.insert cname
    SocketIO.broadcast "client connected" cname

  SocketIO.appendDisconnectHandler $ do
    return ()
    -- mUserName <- liftIO $ STM.atomically $ do
    --   mUserName <- STM.tryReadTMVar userNameMVar
    --   case mUserName of
    --     Just u -> do
    --       connected <- (Set.delete u) . uConnected <$> STM.readTVar (ssConnected state)
    --       typing <- (Set.delete u) . uTyping <$> STM.readTVar (ssTyping state)
    --       STM.writeTVar (ssConnected state) (UsersConnected connected)
    --       STM.writeTVar (ssTyping state) (UsersTyping typing)
    --     Nothing -> return ()
    --   return mUserName

    -- case mUserName of
    --   Nothing -> return ()
    --   Just userName ->
    --     SocketIO.broadcast "user left" (UserJoined userName)
