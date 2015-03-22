{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Present.Model where

import Control.Lens
import Data.Typeable
import Present.Types
import Present.Stack
import Present.Layout
import           Data.Aeson      (FromJSON, ToJSON)
import           Data.Map.Strict (Map)
import           Data.Set        (Set)
import           Data.Text       (Text)
import           GHC.Generics    (Generic)


--------------------------------------------------------------------------------
type SlideSet m = StackSet SectionName (Layout m) m ScreenId ScreenDetail

data AppState sc m = AppState {
    _slides      :: SlideSet sc
  , _application :: m
  } deriving Show

makeLenses ''AppState


slideModel
  :: (Typeable model, Typeable sc, LayoutClass SlideLayout sc)
  => SlideEvent edom
  -> AppState sc model
  -> AppState sc model
slideModel PrevSlide as = as & slides %~ focusUp
slideModel NextSlide as = as & slides %~ focusDown
slideModel (ChangeLayout o) as = as & slides %~ setLayout
  where
    setLayout ss@(StackSet { current = c@(Screen { workspace = ws })}) =
      let l = case (fromLayout . layout $ ws) of
                Just sl -> Layout $ sl { _orientation = o }
                Nothing -> layout ws
      in ss {current = c { workspace = ws { layout = l } } }
slideModel (ToSection s) ss = ss & slides %~ greedyView s
slideModel _ ss = ss
--------------------------------------------------------------------------------
type Uname = Text

type LayoutDetails = Text
type ConnectedState = StackSet SectionName LayoutDetails UsersConnected ScreenId ScreenDetail
type SessionName = Text
type SessionMap = Map SessionName ConnectedState

newtype UsersConnected = UsersConnected { uConnected :: Set Uname } deriving (Show, Generic)

makeWrapped ''UsersConnected

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

newtype CreateSession = CreateSession {
  session :: (SessionName, ConnectedState)
  } deriving (Show, Generic)

instance ToJSON CreateSession
instance FromJSON CreateSession

type ClientId = Text

newtype ClientSession = ClientSession {
  cSession :: (SessionName, Uname)
  } deriving (Show, Generic)

instance ToJSON ClientSession
instance FromJSON ClientSession
