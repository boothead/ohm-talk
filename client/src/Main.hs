{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main where

import qualified Data.Map      as Map
import           Data.String   (fromString)
import qualified Data.Text     as T
import           MVC
import           Ohm.Component
import           Ohm.HTML
import           Ohm.KeyMaster
import           Prelude       hiding (div, filter, id, map, span)
import           Present
import           Slide


-- addClass p (i, s) = (s', rect)
--   where s' = s &~ (classes .= [c])
--         rect = Rectangle 0 0 0 0

introSection :: [Slide () (SlideCommand ()) ]
introSection = mdSlide : [Slide (T.pack $ show t) (slideText t) Nothing "" | t <- [(1::Int)..5]]
  where slideText i = Plain $ into h2_ [fromString $ "intro " ++ show i]
        mdSlide = Slide ("Markdown") (MDFile "mdtest.md") Nothing "A Note"

intro :: SlideSpace () (SlideCommand ())
intro = Workspace "intro" (Layout $ SlideLayout 900 600 V) (differentiate introSection)

problemSection :: [Slide () (SlideCommand ()) ]
problemSection = [Slide (T.pack $ show t) (slideText t) Nothing "" | t <- [(1::Int)..5]]
  where slideText i = Plain $ into h2_ [fromString $ "problem " ++ show i]


problem :: SlideSpace () (SlideCommand ())
problem = Workspace "problem" (Layout $ SlideLayout 900 600 V) (differentiate problemSection)

ss :: SlideState () (SlideCommand ())
ss = StackSet (scrn intro) [] [problem] Map.empty
  where scrn slides' = Screen slides' (S 0) (SD $ Rectangle 0 0 900 600)

main :: IO ()
main = do
  _ <- initDomDelegator
  km <- initKeyMaster
  (keySink, keySource) <- spawn unbounded
  withKeys km keySink [
      ("left", PrevSlide)
    , ("right", NextSlide)
    , ("v", ChangeLayout (Layout $ SlideLayout 900 600 V))
    , ("h", ChangeLayout (Layout $ SlideLayout 900 600 H))
    , ("1", ToSection "intro")
    , ("2", ToSection "problem")
    ]
  modelSink <- runComponent (AppState ss ()) () slideComponent
  forkProcessor () $ for (fromInput keySource) (runProcessor $ domEventsProcessor slideComponent)
               >-> (toOutput modelSink)
