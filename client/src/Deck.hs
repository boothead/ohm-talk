{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Deck (
    deck

  ) where

import qualified Data.Map      as Map
import           Data.String   (fromString)
import qualified Data.Text     as T
import           Ohm.HTML
import           Present
import           Present.Types
import           Slide


introSection :: [SC SModel Edom ]
introSection = mdExtSlide : mdSlide : (slideText <$> [(1::Int)..5])
  where slideText (("intro " ++) . show -> title') =
           plainSlide (T.pack title') $ into h2_ [fromString title']
        mdExtSlide = externalSlide "Markdown" "mdtest.md"
        mdSlide = inlineSlide "Inline" md
        md = T.pack $ unlines [
                 "# Inline test"
               , ""
               , "Blah"
               ]

intro :: SlideSpace SModel Edom
intro = Workspace "intro" (Layout $ SlideLayout 900 600 V) (differentiate introSection)


problemSection :: [SC SModel Edom]
problemSection = bgSlide "./img/component.png" "Component" : (slideText <$> [(1::Int)..5])
  where slideText (("intro " ++) . show -> title') =
           plainSlide (T.pack title') $ into h2_ [fromString title']

problem :: SlideSpace SModel Edom
problem = Workspace "problem" (Layout $ SlideLayout 900 600 V) (differentiate problemSection)

deck :: SlideState SModel Edom
deck = StackSet (scrn intro) [] [problem] Map.empty
  where scrn slides' = Screen slides' (S 0) (SD $ Rectangle 0 0 900 600)
