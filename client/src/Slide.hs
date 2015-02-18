{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}

module Slide where

import           Control.Lens
--import           Data.Aeson   (FromJSON, ToJSON)
--import           Data.Aeson   as Aeson
import           Data.Text    (Text)
import qualified Data.Text    as T
import           GHC.Generics (Generic)
import           Ohm.HTML
import           Present


type SectionName = String

data SlidePosition = Past | Present | Future deriving (Show, Generic)

data Slide a = Slide {
    _title :: Text
  , _content  :: a
  , _position :: Maybe SlidePosition
  , _notes :: Text
  } deriving (Show, Generic, Functor)

makeLenses ''Slide

data SlideContent = Plain HTML
                  | forall a. Pointer a (a -> HTML)

instance Show SlideContent where
  show _ = "SC"

--makePrisms ''SlideContent
type SC = Slide SlideContent

renderSlideContent :: SlideContent -> HTML
renderSlideContent  (Plain h) = h
renderSlideContent (Pointer a f) = f a

renderSlide :: Slide SlideContent -> HTML
renderSlide s = 
  with section
    (classes .= (toCls $ s ^. position))
    (s ^.. content.to renderSlideContent)
  where  
  toCls Nothing = []
  toCls (Just Present) = ["present"]
  toCls (Just Past) = ["past"]
  toCls (Just Future) = ["future"]
