{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Present.Types where

import           Control.Lens
import           Data.Data
import           Data.Int
import           Data.Text    (Text)
import           GHC.Generics (Generic)

--------------------------------------------------------------------------------
type Position = Int32
type Dimension = Int32
type SectionName = Text
type SlideName = Text
type BackgroundValue = Text


-- | Physical screen indices
newtype ScreenId    = S Int deriving (Eq,Ord,Show,Read,Enum,Num,Integral,Real)

-- | The 'Rectangle' with screen dimensions
data ScreenDetail   = SD { screenRect :: !Rectangle } deriving (Eq,Show, Read)

--------------------------------------------------------------------------------
data Rectangle = Rectangle {
    rect_x      :: !Position
  , rect_y      :: !Position
  , rect_width  :: !Dimension
  , rect_height :: !Dimension
  }
  deriving (Eq, Read, Show, Typeable, Data)

data SlidePosition = Past | Present | Future deriving (Show, Generic)

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
data SlideCommand modelEvent =
    NextSlide
  | PrevSlide
  | ToSlide SlideName
  | NextSection
  | PrevSection
  | ToSection SectionName
  | ChangeLayout Orientation
  | Passthrough modelEvent
  deriving (Generic, Typeable, Show)

--------------------------------------------------------------------------------
data Orientation = V | H deriving (Show, Read, Typeable)

data SlideLayout a = SlideLayout {
    _slWidth     :: Dimension
  , _slHeight    :: Dimension
  , _orientation :: Orientation
  } deriving (Show, Read, Typeable)

makeLenses ''SlideLayout
