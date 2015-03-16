{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts           #-}
module Present.Model where

import Control.Lens
import Data.Typeable
import Present.Types
import Present.Stack
import Present.Layout


--------------------------------------------------------------------------------
type SlideState m = StackSet SectionName (Layout m) m ScreenId ScreenDetail

data AppState sc m = AppState {
    _slides      :: SlideState sc
  , _application :: m
  } deriving Show

makeLenses ''AppState


slideModel
  :: (Typeable model, Typeable sc, LayoutClass SlideLayout sc)
  => SlideCommand edom
  -> AppState sc model
  -> AppState sc model
slideModel PrevSlide as = as & slides %~ focusUp
slideModel NextSlide as = as & slides %~ focusDown
slideModel (ChangeLayout o) as = as & slides %~ setLayout
  where
    -- orientationLens = to (fromLayout . layout) . _Just
    -- setLayout
    --   :: Orientation
    --   -> SlideState model edom
      
    --   -> SlideState model edom
    setLayout ss@(StackSet { current = c@(Screen { workspace = ws })}) =
      let l = case (fromLayout . layout $ ws) of
                Just sl -> Layout $ sl { _orientation = o }
                Nothing -> layout ws
      in ss {current = c { workspace = ws { layout = l } } }
slideModel (ToSection s) ss = ss & slides %~ greedyView s
slideModel _ ss = ss
--------------------------------------------------------------------------------
