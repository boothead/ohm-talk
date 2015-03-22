{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Slide where

import           Control.Applicative
import           Control.Lens               hiding (aside)
import qualified Control.Lens               as Lens
--import           Data.Aeson   (FromJSON, ToJSON)
--import           Data.Aeson   as Aeson
import           Control.Monad.State
import           Data.Foldable              (for_, traverse_)
import           Data.String                (fromString)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Lens             as Lens
import           Data.Typeable
import           GHCJS.Foreign
import           MVC hiding (handles)
import           Ohm.Component              (Component (..), Processor (..), handles)
import           Ohm.HTML
import           Present
import           Present.Model
import           Present.Types
import qualified VirtualDom.HTML.Attributes as A
import           VirtualDom.Prim


--------------------------------------------------------------------------------
data SlideContent model edom =
    Plain HTML
  | MD Text
  | MDFile SectionName
  | BGOnly
  | forall b. Pointer (Lens' model b) (Renderer edom b)
  deriving (Typeable)

type SC model edom = Slide (SlideContent model edom)
type SlideState m edom = SlideSet (SC m edom)
type SlideSpace m edom = Workspace SectionName (Layout (SC m edom)) (SC m edom)
type SlideRenderer edom model = Renderer SlideCommand model
type ClientAppState m edom = AppState (SC m edom) m
type Command e = Either e SlideCommand

--------------------------------------------------------------------------------
instance Show (SlideContent a e) where
  show _ = "SC"

instance Show HTML where
  show _ = "<html>"


--------------------------------------------------------------------------------
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


--------------------------------------------------------------------------------
currentOreintation :: (Typeable model, Typeable edom)
                   => ClientAppState model edom
                   -> Maybe Orientation
currentOreintation as = as ^? slides . to (fromLayout . layout . workspace . current) . _Just . orientation


--------------------------------------------------------------------------------
modifySlide :: MonadState HTMLElement m => SlideContent model edom -> m ()
modifySlide (MDFile sec) = do
  attributes.at "data-markdown" ?= toJSString sec
  -- attributes.at "data-separator" ?= "^\n\n\n"
  -- attributes.at "data-separator-vertical" ?= "^\n\n"
  -- attributes.at "data-separator-notes" ?= "^Note:"
  -- attributes.at "data-charset" ?= "iso-8859-15"
modifySlide (MD _) = attributes.at "data-markdown" ?= ""
modifySlide _ = return ()

setSlideBackGround :: (Applicative m, MonadState HTMLElement m) => SlideBG -> m ()
setSlideBackGround (SlideBG bg' trans' nPx') = do
  for_ bg' $ \a -> do
    attributes.at "data-background" ?= toJSString a
    A.style_ ?= (toJSString $ "display: block; background-image: url(" <> a <> ");")
  for_ trans' $ \a ->
    attributes.at "data-background-transition" ?= toJSString (toValue a)
  for_ nPx' $ \a ->
    attributes.at "data-background-size" ?= toJSString (T.pack $ (show a) ++ "px")

setPositionClass :: MonadState HTMLElement m => SC model edom -> m ()
setPositionClass s = A.classes %= (++) (toCls $ s ^. position)
  where toCls = fmap toJSString . positionToClasses


--------------------------------------------------------------------------------
renderSlideContent :: SlideContent model edom -> Renderer edom model
renderSlideContent (Plain h) _ _ = h
renderSlideContent (MD md) _ _ = with script_
                                   (A.type_ ?= "text/template")
                                   [text md]
renderSlideContent (MDFile _) _ _ = div_
renderSlideContent BGOnly _ _ = div_
renderSlideContent (Pointer l r) chan (Lens.view l -> m) = r chan m

renderSlide :: SC model edom -> Renderer edom model
renderSlide s chan mdl =
  with section_ (do
    setPositionClass s
    A.id_ ?= slideKey
    A.style_ ?= "display: block; top: 10;"
    attributes . at "data-transition" ?= "slide"
    key .= slideKey
    modifySlide (s ^. content))
    (render' mdl <$> (s ^.. content))
  where
  slideKey = s ^. title.Lens.unpacked.to fromString
  render' mdl' sc = renderSlideContent sc chan mdl'

renderSlideBackground :: SC model edom -> HTML
renderSlideBackground s = editing div_ $ do
  A.classes .= ["slide-background"]
  setPositionClass s
  setSlideBackGround (s ^. background)

renderSlideSet :: (Typeable model, Typeable edom)
               => Renderer (Command edom) (ClientAppState model edom)
renderSlideSet chan app@(AppState ss mdl) =
  let ws = workspace . current $ ss
      sr = screenRect . screenDetail . current $ ss
      recs = maybe [] (pureLayout (layout ws) sr) (stack ws)
      renderSlide' (s, r) =
        let slideHTML = renderSlide s (contramap Left chan) mdl
            dims = styles r
        in editing slideHTML (A.style_ %= fmap (toJSString.(++dims).fromJSString))
      styles r = unwords [ "width:"
                         , (show . rect_width $ r) ++ "px;"
                         , "height:"
                         , (show . rect_height $ r) ++ "px;"
                         ]
      slides' = renderSlide' <$> recs
      sl = currentOreintation app
      children' = maybe slides' setOrientation sl
        where
        setOrientation V =
           [with section_ (A.classes .= ["stack", "present"])
              slides']
        setOrientation H = slides'
      slideBackgrounds = renderSlideBackground . fst <$> recs
      el = with div_ (A.classes .= ["reveal"])
             [ with div_ (do
                 A.classes .= ["slides"]
                 A.style_ ?= (fromString $ "display: block;" ++ styles sr))
                 children'
             , with div_ (A.classes .= ["backgrounds"])
                 slideBackgrounds
             , renderSlideControls (contramap Right chan) app
             ]
  in el

renderSlideControls :: Renderer SlideCommand (ClientAppState model edom)
renderSlideControls chan (_slides -> ss) =
  with aside_ (do
     A.classes .= ["controls"]
     A.style_ ?= "display: block"
     )
    [control dir cmd f | (dir, cmd, f) <- arrows]
  where
  ws = workspace . current $ ss
  arrows = [ (("left" :: String), CPrevSlide, up)
           , ("right", CNextSlide, down)
           -- , ("up", False)
           -- , ("down", False)
           ]
  control dir cmd f =
    with div_ (do
      A.classes .= [toJSString $ "navigate-" ++ dir]
      traverse_ (enabled cmd . f) $ stack ws)
      []
  --enabled :: (Command edom) model -> [SC model ((Command edom) model)] -> State HTML ()
  enabled cmd slides' = do
    case slides' of
      [] -> onClick $ DOMEvent (const $ putStrLn "Click prevented")
      _  -> do
        A.classes %= ("enabled":)
        onClick $ contramap (const cmd) chan


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


--------------------------------------------------------------------------------
-- Component
data SModel = SModel deriving (Show, Typeable)

data Edom = Edom deriving (Show, Typeable)

slideComponent
  :: Component env (SlideEvent Edom)
                  (ClientAppState SModel Edom)
                  (Command Edom)
slideComponent = Component slideModel renderSlideSet processBoth
  where
    processBoth = handles _Right slideProcessor


--------------------------------------------------------------------------------
-- DSL

bgSlide :: BackgroundValue -> SlideName -> SC model edom
bgSlide bv name = Slide name BGOnly (bgImage .~ Just bv $ defaultSlideBG) Nothing ""

plainSlide :: SlideName -> HTML -> SC model edom
plainSlide name html = Slide name (Plain html) defaultSlideBG Nothing ""

externalSlide :: SlideName -> Text -> SC model edom
externalSlide name path = Slide name (MDFile path) defaultSlideBG Nothing ""

inlineSlide :: SlideName -> Text -> SC model edom
inlineSlide name md = Slide name (MD md) defaultSlideBG Nothing ""
