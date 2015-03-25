{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Deck (
    deck

  ) where

import           Data.String   (fromString)
import qualified Data.Text     as T
import           Ohm.HTML
import           Present
import           Present.Types
import           Slide


intros = [
    bgSlide "img/intro-title.png" "Welcome"
  , externalSlide "fynder" "md/fynder.md"
  , externalSlide "oo-frameworks" "md/oo-frameworks.md"
  , bgSlide "img/wat-dog.jpg" "wat dog"
  , externalSlide "angular" "md/angular.md"
  , bgSlide "img/angular.png" "angular"
  , bgSlide "img/window-lick.jpg" "javascript"
  , externalSlide "om" "md/om.md"
  ]

ohms = [
    externalSlide "ohm" "md/ohm.md"
  , externalSlide "ohm-vdom" "md/ohm-vdom.md"
  , externalSlide "ohm-composition" "md/ohm-composition.md"
  , externalSlide "ohm-dataflow" "md/ohm-dataflow.md"
  , bgSlide "img/component.png" "oHm component"
  ]

concepts = [
    externalSlide "MVC" "md/MVC.md"
  , externalSlide "Model" "md/Model.md"
  , externalSlide "Renderer" "md/Renderer.md"
  , externalSlide "Commands" "md/Commands.md"
  , externalSlide "Processor" "md/Processor.md"
  , externalSlide "ProcessorOps" "md/ProcessorOps.md"
  ]

alternatives = [
    externalSlide "Francium" "md/Francium.md"
  , externalSlide "Lei" "md/Lei.md"
  , externalSlide "ReactHaskell" "md/ReactHaskell.md"
  ]

intro :: SlideSpace SModel Edom
intro = toSection "intro" H intros

deck :: SlideState SModel Edom
deck = StackSet (scrn intro) [] otherSections
  where
    scrn slides' = Screen slides' (S 0) (SD $ Rectangle 0 0 1000 700)
    otherSections = [toSection t o slides
                      | (t, o, slides) <- [
                         ("oHm", V, ohms)
                       , ("Concepts", H, concepts)
                       , ("Alternatives", H, alternatives)
                                         ]]
