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
import           Ohm.Component
import           Ohm.HTML
import           Prelude                hiding (div, filter, id, map, span)
import           Present
import           Slide


-- addClass p (i, s) = (s', rect)
--   where s' = s &~ (classes .= [c])
--         rect = Rectangle 0 0 0 0

deck :: [Slide () (SlideCommand ()) ]
deck = [Slide (T.pack $ show t) (slideText t) Nothing "" | t <- [1..5]]
  where slideText i = Plain $ into h2 [fromString $ "testing " ++ show i]

ws :: SlideSpace () (SlideCommand ())
ws = Workspace "ws" (Layout $ SlideLayout 900 600 V) (differentiate deck)

ss :: SlideState () (SlideCommand ())
ss = StackSet c [] [] Map.empty
  where c = Screen ws (S 0) (SD $ Rectangle 0 0 900 600)

main = do
  void $ runComponent (AppState ss ()) () slideComponent
