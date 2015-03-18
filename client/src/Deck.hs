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
introSection = mdExtSlide : mdSlide : [Slide (T.pack $ show t) (slideText t) Nothing "" | t <- [(1::Int)..5]]
  where slideText (("intro " ++) . show -> title) =
          plainSlide title $ into h2_ [fromString title]
        mdExtSlide = Slide "Markdown" (MDFile "mdtest.md") Nothing "A Note"
        mdSlide = Slide "Inline" (MD md) Nothing "Inline note"
        md = T.pack $ unlines [
                 "# Inline test"
               , ""
               , "Blah"
               ]

intro :: SlideSpace SModel Edom
intro = Workspace "intro" (Layout $ SlideLayout 900 600 V) (differentiate introSection)


problemSection :: [SC SModel Edom]
problemSection = [Slide (T.pack $ show t) (slideText t) Nothing "" | t <- [(1::Int)..5]]
  where slideText i = Plain $ into h2_ [fromString $ "problem " ++ show i]

problem :: SlideSpace SModel Edom
problem = Workspace "problem" (Layout $ SlideLayout 900 600 V) (differentiate problemSection)

deck :: SlideState SModel Edom
deck = StackSet (scrn intro) [] [problem] Map.empty
  where scrn slides' = Screen slides' (S 0) (SD $ Rectangle 0 0 900 600)
