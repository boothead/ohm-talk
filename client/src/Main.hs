{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}

module Main where

import           Control.Lens
import           Control.Monad.IO.Class (liftIO)
import Control.Monad (void)
import           Ohm.HTML
import           Ohm.Component
import           Prelude                hiding (div, filter, id, map, span)
import           Present
import           Slide
import qualified Data.Map as Map
import qualified Data.Text as T
import GHCJS.Types
import GHCJS.Foreign


-- addClass p (i, s) = (s', rect)
--   where s' = s &~ (classes .= [c])
--         rect = Rectangle 0 0 0 0
                  
slides :: [Slide () (SlideCommand ()) ]
slides = [Slide (T.pack $ show t) (Plain e) Nothing "" | t <- [1..5]]
  where e = into h2 ["testing"]

ws :: SlideSpace () (SlideCommand ())
--ws = Workspace "ws" (SlideLayout 1 V) (differentiate slides)
ws = Workspace "ws" (Layout $ SlideLayout 1 V) (differentiate slides)

ss :: SlideState () (SlideCommand ())
ss = StackSet c [] [] Map.empty
  where c = Screen ws (S 0) (SD $ Rectangle 0 0 1000 800)
  
main = do
  print ss
  void $ runComponent (AppState ss ()) () slideComponent 
