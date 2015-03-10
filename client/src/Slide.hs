{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
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
import           Control.Lens               hiding (aside)
import qualified Control.Lens               as Lens
--import           Data.Aeson   (FromJSON, ToJSON)
--import           Data.Aeson   as Aeson
import           Control.Monad.State
import           Data.Foldable              (traverse_)
import           Data.String                (fromString)
import           Data.Text                  (Text)
import qualified Data.Text.Lens             as Lens
import           Data.Typeable
import           GHC.Generics               (Generic)
import           GHCJS.Foreign
import           MVC
import           Ohm.Component              (Component (..), Processor (..))
import           Ohm.HTML
import           Present
import qualified VirtualDom.HTML.Attributes as A
import           VirtualDom.Prim

type SectionName = Text
type SlideName = Text

data SlidePosition = Past | Present | Future deriving (Show, Generic)

data SlideContent m edom =
    Plain HTML
  | MD Text
  | MDFile SectionName
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

type SC m edom = Slide m edom

data SlideCommand m =
    NextSlide
  | PrevSlide
  | ToSlide SlideName
  | NextSection
  | PrevSection
  | ToSection SectionName
  | ChangeLayout (Layout (SC m (SlideCommand m)))
  deriving (Show, Generic, Typeable)

data Orientation = V | H deriving (Show, Read, Typeable)

data SlideLayout a = SlideLayout {
    _slWidth     :: Dimension
  , _slHeight    :: Dimension
  , _orientation :: Orientation
  } deriving (Show, Read, Typeable)

makeLenses ''SlideLayout


type SlideState m edom = StackSet SectionName (Layout (SC m edom)) (SC m edom) ScreenId ScreenDetail
type SlideSpace m edom = Workspace SectionName (Layout (SC m edom)) (SC m edom)


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
  pureLayout (SlideLayout w h _o) _r s = pasts ++ [f] ++ futures
    where pasts    = mkPast   <$> (zip [0..] $ up s)
          futures  = mkFuture <$> (zip [0..] $ down s)
          f        = (focus s & position .~ Just Present, Rectangle 0 0 w h)
          mkFuture = setPosition Future
          mkPast   = setPosition Past
          setPosition :: SlidePosition -> (Int, SC m edom) -> (SC m edom, Rectangle)
          setPosition p (_i, sc) = (sc & position .~ Just p, rect)
            where rect = Rectangle 0 0 w h

data AppState m edom = AppState {
    _slides      :: SlideState m edom
  , _application :: m
  } deriving Show

makeLenses ''AppState


currentOreintation :: (Typeable a, Typeable e) => AppState a e -> Maybe Orientation
currentOreintation as = as ^? slides . to (fromLayout . layout . workspace . current) . _Just . orientation

--------------------------------------------------------------------------------

slideModel
  :: SlideCommand model
  -> AppState model (SlideCommand model)
  -> AppState model (SlideCommand model)
slideModel PrevSlide as = as & slides %~ focusUp
slideModel NextSlide as = as & slides %~ focusDown
slideModel (ChangeLayout lo) as = as & slides %~ setLayout lo
  where
    setLayout
      :: Layout (SC model (SlideCommand model))
      -> SlideState model (SlideCommand model)
      -> SlideState model (SlideCommand model)
    setLayout l ss@(StackSet { current = c@(Screen { workspace = ws })}) =
      ss {current = c { workspace = ws { layout = l } } }
slideModel (ToSection s) ss = ss & slides %~ greedyView s
slideModel _ ss = ss
--------------------------------------------------------------------------------

type SlideRenderer model = Renderer (SlideCommand model) model

renderSlideContent :: SlideContent model (SlideCommand model) -> SlideRenderer model
renderSlideContent (Plain h) _ _ = h
renderSlideContent (MD md) _ _ = text md
renderSlideContent (MDFile _) _ _ = div_
renderSlideContent (Pointer l r) chan (Lens.view l -> m) = r chan m

modifySlide :: MonadState HTMLElement m => (SlideContent model (SlideCommand model)) -> m ()
modifySlide (MDFile sec) = do
  attributes.at "data-markdown" ?= toJSString sec
  attributes.at "data-separator" ?= "^\n\n\n"
  attributes.at "data-separator-vertical" ?= "^\n\n"
  attributes.at "data-separator-notes" ?= "^Note:"
  attributes.at "data-charset" ?= "iso-8859-15"
modifySlide _ = return ()

renderSlide :: Slide model (SlideCommand model) -> SlideRenderer model
renderSlide s chan mdl =
  with section_ (do
    A.classes .= (toCls $ s ^. position)
    A.id_ ?= slideKey
    A.style_ ?= "display: block; top: 10;"
    attributes . at "data-transition" ?= "slide"
    key .= slideKey
    modifySlide (s ^. content))
    (render' mdl <$> (s ^.. content))
  where
  slideKey = s ^. title.Lens.unpacked.to fromString
  toCls Nothing = []
  toCls (Just Present) = ["present"]
  toCls (Just Past) = ["past"]
  toCls (Just Future) = ["future"]
  render' mdl' sc = renderSlideContent sc chan mdl'

renderSlideSet :: Typeable model
               => Renderer (SlideCommand model) (AppState model (SlideCommand model))
renderSlideSet chan app@(AppState ss mdl) =
  let ws = workspace . current $ ss
      sr = screenRect . screenDetail . current $ ss
      recs = maybe [] (pureLayout (layout ws) sr) (stack ws)
      render' mdl' (s, r) =
        let slideHTML = renderSlide s chan mdl'
            dims = styles r
        in editing slideHTML (A.style_ %= fmap (toJSString.(++dims).fromJSString))
      styles r = unwords [ "width:"
                         , (show . rect_width $ r) ++ "px;"
                         , "height:"
                         , (show . rect_height $ r) ++ "px;"
                         ]
      slides' = render' mdl <$> recs
      sl = currentOreintation app
      children' = maybe slides' setOrientation sl
        where
        setOrientation V =
           [with section_ (A.classes .= ["stack", "present"])
              slides']
        setOrientation H = slides'
      el = with div_ (A.classes .= ["reveal"])
             [ with div_ (do
                 A.classes .= ["slides"]
                 A.style_ ?= (fromString $ "display: block;" ++ styles sr))
                 children'
             , renderSlideControls chan app
             ]
  in el

renderSlideControls :: Renderer (SlideCommand model) (AppState model (SlideCommand model))
renderSlideControls chan (_slides -> ss) =
  with aside_ (do
     A.classes .= ["controls"]
     A.style_ ?= "display: block"
     )
    [control dir cmd f | (dir, cmd, f) <- arrows]
  where
  ws = workspace . current $ ss
  arrows = [ (("left" :: String), PrevSlide, up)
           , ("right", NextSlide, down)
           -- , ("up", False)
           -- , ("down", False)
           ]
  control dir cmd f =
    with div_ (do
      A.classes .= [toJSString $ "navigate-" ++ dir]
      traverse_ (enabled cmd . f) $ stack ws)
      []
  --enabled :: SlideCommand model -> [SC model (SlideCommand model)] -> State HTML ()
  enabled cmd slides' = do
    case slides' of
      [] -> onClick $ DOMEvent (const $ putStrLn "Click prevented")
      _  -> do
        A.classes %= ("enabled":)
        onClick $ contramap (const cmd) chan

slideProcessor :: MonadIO m => Processor m (SlideCommand model) (SlideCommand model)
slideProcessor = Processor $ \cmd -> do
      liftIO $ print cmd
      yield cmd

slideComponent
  :: Typeable model
  => Component env (SlideCommand model)
                   (AppState model (SlideCommand model))
                   (SlideCommand model)
slideComponent = Component slideModel renderSlideSet slideProcessor
