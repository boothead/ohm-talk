{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Slide where

import           Control.Lens hiding (aside)
import qualified Control.Lens as Lens
--import           Data.Aeson   (FromJSON, ToJSON)
--import           Data.Aeson   as Aeson
import           Data.Text    (Text)
import qualified Data.Text    as T
import           GHC.Generics (Generic)
import           Ohm.HTML
import Ohm.Component (Component(..), idProcessor)
import           Present
import           Prelude                hiding (div, filter, id, map, span)
import Debug.Trace

type SectionName = Text
type SlideName = Text

data SlidePosition = Past | Present | Future deriving (Show, Generic)
  
data SlideContent m edom = 
    Plain HTML
  | forall b. Pointer (Lens' m b) (Renderer edom b) 

instance Show (SlideContent a e) where
  show _ = "SC"

data Slide m edom = Slide {
    _title :: SlideName
  , _content  :: SlideContent m edom
  , _position :: Maybe SlidePosition
  , _notes :: Text
  } deriving (Show, Generic)

makeLenses ''Slide

data SlideCommand m =
    NextSlide
  | PrevSlide
  | ToSlide SlideName
  | NextSection
  | PrevSection
  | ToSection SectionName
  | ChangeLayout (SlideLayout (SC m (SlideCommand m)))
  deriving (Show, Generic)

data Orientation = V | H deriving (Show, Read)

data SlideLayout a = SlideLayout {
    _slWidth :: Double
  , _orientation :: Orientation
  } deriving (Show, Read)


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

slideModel :: SlideCommand m -> AppState m edom -> AppState m edom
slideModel = undefined

renderSlideContent :: SlideContent m edom -> Renderer edom m
renderSlideContent (Plain h) _ _ = h
renderSlideContent (Pointer l r) chan (Lens.view l -> m) = r chan m

renderSlide :: Slide m edom -> DOMEvent edom -> m -> HTML
renderSlide s chan model = 
  with section
    (classes .= (toCls $ s ^. position))
    (render chan model <$> (s ^.. content))
  where  
  toCls Nothing = []
  toCls (Just Present) = ["present"]
  toCls (Just Past) = ["past"]
  toCls (Just Future) = ["future"]
  render chan model sc = renderSlideContent sc chan model

renderSlideSet :: Renderer edom (AppState model edom)
renderSlideSet chan app@(AppState ss model) =
  let ws = workspace . current $ ss
      r = screenRect . screenDetail . current $ ss
      recs = maybe [] (pureLayout (layout ws) r) (stack ws)
      render chan model s = renderSlide s chan model
      el = with div (classes .= ["reveal"])
             [ with div (classes .= ["slides"])
                (fmap  (render chan model . fst) recs)
             , renderSlideControls chan app
             ] 
  in el
  
renderSlideControls :: Renderer edom (AppState model edom)
renderSlideControls chan (_slides -> ss) =
  with aside (do
     classes .= ["controls"]
     attrs.at "style" ?= "display: block"
     )
    [control dir cmd | (dir, cmd) <- arrows]
  where
  ws = workspace . current $ ss
  arrows = [ ("left", PrevSlide)
           , ("right", NextSlide)
           -- , ("up", False)
           -- , ("down", False)
           ]
  control dir cmd =
    with div
       (classes .= (["navigate-" ++ dir] ++ (maybe [] (enabled cmd) (stack ws))))
       []
  enabled :: SlideCommand model -> Stack (SC model edom) -> [String]
  enabled PrevSlide (Stack _ up _) = if null up then ["enabled"] else []
  enabled NextSlide (Stack _ _ down) = if null down then ["enabled"] else []
  enabled _ _ = []

--slideComponent :: Component () (SlideCommand ()) A
slideComponent = Component slideModel renderSlideSet idProcessor
