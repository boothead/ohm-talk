{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}

module Main where

import           Control.Lens
import           Control.Monad.IO.Class (liftIO)
import           Ohm.HTML
import           Prelude                hiding (div, filter, id, map, span)
import           Present
import           Slide
import qualified Data.Text as T
import GHCJS.Types
import GHCJS.Foreign


type SlideState = StackSet WorkspaceId (Layout SC) SC ScreenId ScreenDetail
type SlideSpace = Workspace WorkspaceId (Layout SC) SC

-- | Physical screen indices
newtype ScreenId    = S Int deriving (Eq,Ord,Show,Read,Enum,Num,Integral,Real)

-- | The 'Rectangle' with screen dimensions
data ScreenDetail   = SD { screenRect :: !Rectangle } deriving (Eq,Show, Read)

instance Show HTML where
  show _ = "<html>"


instance LayoutClass Layout SC where
    runLayout (Workspace i (Layout l) ms) r = fmap (fmap Layout) `fmap` runLayout (Workspace i l ms) r
    doLayout (Layout l) r s  = fmap (fmap Layout) `fmap` doLayout l r s
    emptyLayout (Layout l) r = fmap (fmap Layout) `fmap` emptyLayout l r
    handleMessage (Layout l) = fmap (fmap Layout) . handleMessage l
    description (Layout l)   = description l

data AppState a = AppState {
    _slides :: SlideState
  , _app    :: a
  } deriving Show

data Orientation = V | H deriving (Show, Read)

data SlideLayout a = SlideLayout {
    _slWidth :: Double
  , _orientation :: Orientation
  } deriving (Show, Read)
  
instance LayoutClass SlideLayout SC where
  pureLayout (SlideLayout w o) r s = pasts ++ [f] ++ futures
    where pasts    = mkPast   <$> (zip [0..] $ up s)
          futures  = mkFuture <$> (zip [0..] $ down s)
          f        = (focus s & position .~ Just Present, Rectangle 0 0 0 0)
          mkFuture = setPosition Future
          mkPast   = setPosition Past
          setPosition p (i, s) = (s & position .~ Just p, rect)
            where rect = Rectangle 0 0 0 0

-- addClass p (i, s) = (s', rect)
--   where s' = s &~ (classes .= [c])
--         rect = Rectangle 0 0 0 0
                  
slides :: [Slide SlideContent]
slides = [Slide (T.pack $ show t) (Plain e) Nothing "" | t <- [1..5]]
  where e = into h2 ["test"]

ws :: SlideSpace
ws = Workspace "ws" (Layout $ SlideLayout 1 V) (differentiate slides)

l = runLayout ws (Rectangle 0 0 100 100)

main = do
  (recs, _) <- liftIO l
  let el = with div (classes .= ["reveal"])
             [ with div (classes .= ["slides"])
                 (fmap  (renderSlide.fst) recs)
             ]
  c <- newTopLevelContainer
  renderTo c el

