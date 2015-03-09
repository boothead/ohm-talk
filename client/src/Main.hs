{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main where

import           Control.Lens
import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Map               as Map
import           Data.String            (fromString)
import qualified Data.Text              as T
import           GHCJS.Foreign
import           GHCJS.Types
import           MVC
import           Ohm.Component
import           Ohm.HTML
import           Ohm.KeyMaster
import           Prelude                hiding (div, filter, id, map, span)
import           Present
import           Slide


-- addClass p (i, s) = (s', rect)
--   where s' = s &~ (classes .= [c])
--         rect = Rectangle 0 0 0 0

deck :: [Slide () (SlideCommand ()) ]
deck = [Slide (T.pack $ show t) (slideText t) Nothing "" | t <- [1..5]]
  where slideText i = Plain $ into h2_ [fromString $ "testing " ++ show i]

ws :: SlideSpace () (SlideCommand ())
ws = Workspace "ws" (Layout $ SlideLayout 900 600 V) (differentiate deck)

ss :: SlideState () (SlideCommand ())
ss = StackSet c [] [] Map.empty
  where c = Screen ws (S 0) (SD $ Rectangle 0 0 900 600)

main = do
  _ <- initDomDelegator
  km <- initKeyMaster
  (keySink, keySource) <- spawn unbounded
  withKeys km keySink [
      ("left", PrevSlide)
    , ("right", NextSlide)
    , ("v", ChangeLayout (Layout $ SlideLayout 900 600 V))
    , ("h", ChangeLayout (Layout $ SlideLayout 900 600 H))
    ]
  modelSink <- runComponent (AppState ss ()) () slideComponent
  forkProcessor () $ for (fromInput keySource) (runProcessor $ domEventsProcessor slideComponent)
               >-> (toOutput modelSink)
