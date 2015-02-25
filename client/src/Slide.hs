{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Slide where

import           Control.Applicative
import           Control.Lens        hiding (aside)
import qualified Control.Lens        as Lens
--import           Data.Aeson   (FromJSON, ToJSON)
--import           Data.Aeson   as Aeson
import           Control.Monad.State
import           Data.Foldable       (traverse_)
import           Data.String         (fromString)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.Lens      as Lens
import           Data.Typeable
import           Debug.Trace
import           GHC.Generics        (Generic)
import           MVC
import           Ohm.Component       (Component (..), Processor (..),
                                      idProcessor)
import           Ohm.HTML            hiding (title)
import           Prelude             hiding (div, filter, id, map, span)
import qualified Prelude
import           Present

type SectionName = Text
type SlideName = Text

data SlidePosition = Past | Present | Future deriving (Show, Generic)

data SlideContent m edom =
    Plain HTML
  | forall b. Pointer (Lens' m b) (Renderer edom b)

instance Show (SlideContent a e) where
  show _ = "SC"

data Slide m edom = Slide {
    _title    :: SlideName
  , _content  :: SlideContent m edom
  , _position :: Maybe SlidePosition
  , _notes    :: Text
  } deriving (Show, Generic, Typeable)

makeLenses ''Slide

data SlideCommand m =
    NextSlide
  | PrevSlide
  | ToSlide SlideName
  | NextSection
  | PrevSection
  | ToSection SectionName
  | ChangeLayout (SlideLayout (SC m (SlideCommand m)))
  deriving (Show, Generic, Typeable)

data Orientation = V | H deriving (Show, Read, Typeable)

data SlideLayout a = SlideLayout {
    _slWidth     :: Double
  , _orientation :: Orientation
  } deriving (Show, Read, Typeable)


type SC m edom = Slide m edom

type SlideState m edom = StackSet WorkspaceId (Layout (SC m edom)) (SC m edom) ScreenId ScreenDetail
type SlideSpace m edom = Workspace WorkspaceId (Layout (SC m edom)) (SC m edom)


--makePrisms ''SlideContent

-- | Physical screen indices
newtype ScreenId    = S Int deriving (Eq,Ord,Show,Read,Enum,Num,Integral,Real)

-- | The 'Rectangle' with screen dimensions
data ScreenDetail   = SD { screenRect :: !Rectangle } deriving (Eq,Show, Read)

instance Show HTML where
  show _ = "<html>"


instance LayoutClass Layout (SC m edom) where
    pureLayout (Layout l) = pureLayout l
    runLayout (Workspace i (Layout l) ms) r = fmap (fmap Layout) `fmap` runLayout (Workspace i l ms) r
    doLayout (Layout l) r s  = fmap (fmap Layout) `fmap` doLayout l r s
    emptyLayout (Layout l) r = fmap (fmap Layout) `fmap` emptyLayout l r
    handleMessage (Layout l) = fmap (fmap Layout) . handleMessage l
    description (Layout l)   = description l


instance LayoutClass SlideLayout (SC m edom) where
  pureLayout (SlideLayout w o) r s = pasts ++ [f] ++ futures
    where pasts    = mkPast   <$> (zip [0..] $ up s)
          futures  = mkFuture <$> (zip [0..] $ down s)
          f        = (focus s & position .~ Just Present, Rectangle 0 0 0 0)
          mkFuture = setPosition Future
          mkPast   = setPosition Past
          setPosition :: SlidePosition -> (Int, SC m edom) -> (SC m edom, Rectangle)
          setPosition p (i, s) = (s & position .~ Just p, rect)
            where rect = Rectangle 0 0 0 0


data AppState m edom = AppState {
    _slides :: SlideState m edom
  , _app    :: m
  } deriving Show

makeLenses ''AppState

slideModel :: SlideCommand model -> AppState model (SlideCommand model) -> AppState model (SlideCommand model)
slideModel PrevSlide ss = ss & slides %~ focusUp
slideModel NextSlide ss = ss & slides %~ focusDown
slideModel _ ss = ss

renderSlideContent :: SlideContent model (SlideCommand model) -> Renderer (SlideCommand model) model
renderSlideContent (Plain h) _ _ = h
renderSlideContent (Pointer l r) chan (Lens.view l -> m) = r chan m

renderSlide :: Slide model (SlideCommand model) -> DOMEvent (SlideCommand model) -> model -> HTML
renderSlide s chan model =
  with section (do
    classes .= (toCls $ s ^. position)
    attrs.at "id" ?= slideKey
    attrs.at "style" ?= "display: block; top: 10; height:400px;"
    attrs.at "data-transition" ?= "slide"
    setKey slideKey)
    (render chan model <$> (s ^.. content))
  where
  slideKey = s ^. title.Lens.unpacked.to fromString
  toCls Nothing = []
  toCls (Just Present) = ["present"]
  toCls (Just Past) = ["past"]
  toCls (Just Future) = ["future"]
  render chan model sc = renderSlideContent sc chan model

renderSlideSet :: Typeable model
               => Renderer (SlideCommand model) (AppState model (SlideCommand model))
renderSlideSet chan app@(AppState ss model) =
  let ws = workspace . current $ ss
      r = screenRect . screenDetail . current $ ss
      recs = maybe [] (pureLayout (layout ws) r) (stack ws)
      render chan model s = renderSlide s chan model
      styles = unwords [ "width:"
                       , (show . rect_width $ r) ++ "px;"
                       , "height:"
                       , (show . rect_height $ r) ++ "px;"
                       ]
      slides = render chan model . fst <$> recs
      sl = fromLayout $ layout ws
      children = maybe slides setOrientation sl
        where
        setOrientation (SlideLayout _ V) =
           [with section (classes .= ["stack", "present"])
              slides]
        setOrientation (SlideLayout _ H) = slides
      el = with div (classes .= ["reveal"])
             [ with div (do
                 classes .= ["slides"]
                 attrs.at "style" ?= (fromString $ "display: block;" ++ styles))
                 children
             , renderSlideControls chan app
             ]
  in el

renderSlideControls :: Renderer (SlideCommand model) (AppState model (SlideCommand model))
renderSlideControls chan (_slides -> ss) =
  with aside (do
     classes .= ["controls"]
     attrs.at "style" ?= "display: block"
     )
    [control dir cmd f | (dir, cmd, f) <- arrows]
  where
  ws = workspace . current $ ss
  arrows = [ ("left", PrevSlide, up)
           , ("right", NextSlide, down)
           -- , ("up", False)
           -- , ("down", False)
           ]
  control dir cmd f =
    with div (do
      classes .= ["navigate-" ++ dir]
      traverse_ (enabled cmd . f) $ stack ws)
      []
  --enabled :: SlideCommand model -> [SC model (SlideCommand model)] -> State HTML ()
  enabled cmd slides = do
    case slides of
      [] -> onClick $ DOMEvent (const $ putStrLn "Click prevented")
      _  -> do
        classes %= ("enabled":)
        onClick $ contramap (const cmd) chan

slideProcessor :: MonadIO m => Processor m (SlideCommand ()) (SlideCommand ())
slideProcessor = Processor $ \cmd -> do
      liftIO $ print cmd
      yield cmd

--slideComponent :: Component () (SlideCommand ()) A
slideComponent = Component slideModel renderSlideSet slideProcessor
