{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Present.Types where

import           Control.Lens
import           Data.Aeson      (FromJSON, ToJSON)
import           Data.Data
import           Data.Int
-- -- import           Data.Map.Strict (Map)
-- import           Data.Set        (Set)
import           Data.Text       (Text)
import           GHC.Generics    (Generic)
-- import           Present.Stack
-- import qualified Data.Set     as Set


--------------------------------------------------------------------------------
type Position = Int32
type Dimension = Int32
type SectionName = Text
type SlideName = Text
type BackgroundValue = Text



-- | Physical screen indices
newtype ScreenId    = S Int deriving (Eq,Ord,Show,Read,Enum,Num,Integral,Real, Generic)

instance ToJSON ScreenId
instance FromJSON ScreenId

-- | The 'Rectangle' with screen dimensions
data ScreenDetail   = SD { screenRect :: !Rectangle } deriving (Eq,Show, Read, Generic)

instance ToJSON ScreenDetail
instance FromJSON ScreenDetail

--------------------------------------------------------------------------------
data Rectangle = Rectangle {
    rect_x      :: !Position
  , rect_y      :: !Position
  , rect_width  :: !Dimension
  , rect_height :: !Dimension
  }
  deriving (Eq, Read, Show, Typeable, Data, Generic)

instance ToJSON Rectangle
instance FromJSON Rectangle

data SlidePosition = Past | Present | Future deriving (Show, Generic)

positionToClasses :: Maybe SlidePosition -> [String]
positionToClasses Nothing = []
positionToClasses (Just Present) = ["present"]
positionToClasses (Just Past) = ["past"]
positionToClasses (Just Future) = ["future"]


data SlideTransition = STSlide | STFade deriving (Show, Generic)

toValue :: SlideTransition -> String
toValue STSlide = "slide"
toValue STFade = "fade"

data SlideBG = SlideBG {
    _bgImage      :: Maybe BackgroundValue
  , _bgTransition :: Maybe SlideTransition
  , _bgSize       :: Maybe Int
  } deriving (Show, Generic)

makeLenses ''SlideBG

defaultSlideBG :: SlideBG
defaultSlideBG = SlideBG Nothing Nothing Nothing

data Slide a = Slide {
    _title      :: SlideName
  , _content    :: a
  , _background :: SlideBG
  , _position   :: Maybe SlidePosition
  , _notes      :: Text
  } deriving (Show, Generic, Typeable)

makeLenses ''Slide

-- instance Show l => Show (HasLayout l) where
--   show = ("HasLayout "++) . show
--------------------------------------------------------------------------------
data Orientation = V | H deriving (Show, Read, Typeable, Generic)

instance ToJSON Orientation
instance FromJSON Orientation

data SlideLayout a = SlideLayout {
    _slWidth     :: Dimension
  , _slHeight    :: Dimension
  , _orientation :: Orientation
  } deriving (Show, Read, Typeable)

makeLenses ''SlideLayout

--------------------------------------------------------------------------------
data SlideEvent modelEvent =
    NextSlide
  | PrevSlide
  | ToSlide SlideName
  | NextSection
  | PrevSection
  | ToSection SectionName
  | ChangeLayout Orientation
  | Passthrough modelEvent
  deriving (Generic, Typeable, Show)

instance (ToJSON m) => ToJSON (SlideEvent m)
instance (FromJSON m) => FromJSON (SlideEvent m)

data SlideCommand =
    CNextSlide
  | CPrevSlide
  | CToSlide SlideName
  | CNextSection
  | CPrevSection
  | CToSection SectionName
  | CChangeLayout Orientation
  deriving (Generic, Typeable, Show)

instance ToJSON SlideCommand
instance FromJSON SlideCommand

cmdToEvent :: SlideCommand -> Maybe (SlideEvent ())
cmdToEvent CNextSlide = Just NextSlide
cmdToEvent CPrevSlide = Just PrevSlide
cmdToEvent (CToSection s) = Just (ToSection s)
cmdToEvent _ = Nothing

