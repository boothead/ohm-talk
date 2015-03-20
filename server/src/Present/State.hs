{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


module Present.State (
    server
  , ServerState (..)
  , UsersConnected(..)
  , UsersTyping(..)
  ) where

import           Prelude                    hiding (mapM_)

import           Control.Applicative
import qualified Control.Concurrent.STM     as STM
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.State.Strict (StateT)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.Foldable              (mapM_)
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import           Data.Typeable
import           GHC.Generics
import qualified Network.SocketIO           as SocketIO
import           Present.Model
import           Present.Stack
import           Present.Types
import qualified Snap.Core                  as Snap

--------------------------------------------------------------------------------
type Uname = Text
type ConnectedState = SlideSet UsersConnected

newtype UsersConnected = UsersConnected { uConnected :: Set Uname } deriving (Show, Generic)

instance ToJSON UsersConnected
instance FromJSON UsersConnected

newtype Location = Location { unLoc :: (SectionName, SlideName) } deriving (Show, Generic)

instance ToJSON Location
instance FromJSON Location


data SlideStateCommand =
    InitialState ConnectedState
  | CurrentLoc Location
  deriving (Generic, Typeable, Show)

instance ToJSON SlideStateCommand
instance FromJSON SlideStateCommand


--------------------------------------------------------------------------------
data ServerState = ServerState {
   _slideStructure        :: STM.TVar ConnectedState
 , _slideLocation :: STM.TVar Location
 } deriving (Show, ToJSON)

logMsg :: Show a => String -> a -> IO ()
logMsg msg a = putStrLn (msg ++ ": " ++ show a)

getState :: MonadIO m => ServerState -> m (ConnectedState, Location)
getState state = liftIO $ STM.atomically $ do
  s <- _slideStructure <$> STM.readTVar (ssConnected state)
  l <- _slideLocation <$> STM.readTVar (ssTyping state)
  -- m <- STM.readTVar (ssMessages state)
  return $ (s ,l)

server :: ServerState -> StateT SocketIO.RoutingTable (ReaderT SocketIO.Socket Snap.Snap) ()
server state = do
  userNameMVar <- liftIO STM.newEmptyTMVarIO
  let state' = getState state
  let forUserName m = liftIO (STM.atomically (STM.tryReadTMVar userNameMVar)) >>= mapM_ m

  SocketIO.on "controller connect" $ \ss ->
    forUserName $ \userName -> do
      let s = Said userName message
      liftIO $ do
        logMsg "SAID" s
        STM.atomically $ do
          messages <- (s:) <$> STM.readTVar (ssMessages state)
          STM.writeTVar (ssMessages state) messages
      SocketIO.broadcast "new message" s


  SocketIO.on "user connect" $ \(AddUser userName) -> do
    names <- liftIO $ do
      STM.atomically $ do
        names <- (Set.insert userName) . uConnected <$> STM.readTVar (ssConnected state)
        STM.putTMVar userNameMVar userName
        STM.writeTVar (ssConnected state) (UsersConnected names)
        return names

    SocketIO.emit "login" (Loggedin userName)
    SocketIO.broadcast "user joined" (UserJoined userName)
    SocketIO.broadcast "currently connected" names

  SocketIO.appendDisconnectHandler $ do
    mUserName <- liftIO $ STM.atomically $ do
      mUserName <- STM.tryReadTMVar userNameMVar
      case mUserName of
        Just u -> do
          connected <- (Set.delete u) . uConnected <$> STM.readTVar (ssConnected state)
          typing <- (Set.delete u) . uTyping <$> STM.readTVar (ssTyping state)
          STM.writeTVar (ssConnected state) (UsersConnected connected)
          STM.writeTVar (ssTyping state) (UsersTyping typing)
        Nothing -> return ()
      return mUserName

    case mUserName of
      Nothing -> return ()
      Just userName ->
        SocketIO.broadcast "user left" (UserJoined userName)
