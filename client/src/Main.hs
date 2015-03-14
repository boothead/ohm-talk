{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main where

import qualified Data.Map      as Map
import           Data.String   (fromString)
import qualified Data.Text     as T
import           Data.Typeable
import           MVC
import           Ohm.Component
import           Ohm.HTML
import           Ohm.KeyMaster
import           Prelude       hiding (div, filter, id, map, span)
import           Present
import           Present.Types
import           Slide


-- addClass p (i, s) = (s', rect)
--   where s' = s &~ (classes .= [c])
--         rect = Rectangle 0 0 0 0

introSection :: [SC SModel Edom ]
introSection = mdExtSlide : mdSlide : [Slide (T.pack $ show t) (slideText t) Nothing "" | t <- [(1::Int)..5]]
  where slideText i = Plain $ into h2_ [fromString $ "intro " ++ show i]
        mdExtSlide = Slide ("Markdown") (MDFile "mdtest.md") Nothing "A Note"
        mdSlide = Slide "Inline" (MD md) Nothing "Inline note"
        md = T.pack $ unlines [
                 "# Inline test"
               , ""
               , "Blah"
               ]

intro :: SlideSpace SModel Edom
intro = Workspace "intro" (Layout $ SlideLayout 900 600 V) (differentiate introSection)


--problemSection :: [SC ()]
problemSection = [Slide (T.pack $ show t) (slideText t) Nothing "" | t <- [(1::Int)..5]]
  where slideText i = Plain $ into h2_ [fromString $ "problem " ++ show i]

problem :: SlideSpace SModel Edom
problem = Workspace "problem" (Layout $ SlideLayout 900 600 V) (differentiate problemSection)

ss :: SlideState SModel Edom
ss = StackSet (scrn intro) [] [problem] Map.empty
  where scrn slides' = Screen slides' (S 0) (SD $ Rectangle 0 0 900 600)
  
foreign import javascript unsafe
  "RevealMarkdown.initialize()"
  convertMDSlides :: IO ()

main :: IO ()
main = do
  _ <- initDomDelegator
  km <- initKeyMaster
  key km "r" convertMDSlides
  (keySink, keySource) <- spawn unbounded
  withKeys km keySink [
      ("left", PrevSlide)
    , ("right", NextSlide)
    , ("v", ChangeLayout V)
    , ("h", ChangeLayout H)
    , ("1", ToSection "intro")
    , ("2", ToSection "problem")
    ]
  modelSink <- runComponent (AppState ss SModel) () slideComponent
  forkProcessor () $ for (fromInput keySource) (runProcessor $ domEventsProcessor slideComponent)
               >-> (toOutput modelSink)
