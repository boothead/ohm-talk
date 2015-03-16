{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main where


import           MVC
import           Ohm.Component
import           Ohm.HTML
import           Ohm.KeyMaster
import           Prelude       hiding (div, filter, id, map, span)
import           Present.Model
import           Present.Types

import           Deck
import           Slide


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
  modelSink <- runComponent (AppState deck SModel) () slideComponent
  forkProcessor () $ for (fromInput keySource) (runProcessor $ domEventsProcessor slideComponent)
               >-> (toOutput modelSink)
