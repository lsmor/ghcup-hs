{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS_GHC -Wno-unused-record-wildcards #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PatternSynonyms #-}

{-
This module contains common values used across the library. Crucially it contains two important types for the brick app:

- Name: List all resources (widgets) used by the app. see https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst#resource-names
- Mode: Use to dispatch events and drawings. see: https://github.com/jtdaugherty/brick/issues/476#issuecomment-1629151920

-}

module GHCup.Brick.Common where

import           GHCup.List ( ListResult )
import           GHCup.Types ( Tool, KeyCombination (KeyCombination) )
import Data.List (intercalate)
import           Prelude                 hiding ( appendFile )
import qualified Graphics.Vty                  as Vty
import           Optics.TH (makeLenses)
import           Optics.Lens (toLensVL)
import qualified Brick
import qualified Brick.Widgets.Border as Border
import Brick ((<+>))
import qualified Data.Text as T
import qualified Brick.Widgets.Center as Brick
import qualified Brick.Widgets.Border.Style as Border

-- We could use regular ADTs but different menus share the same options.
-- example: all of ghcup compile ghc, ghcup compile hls, ghcup install cabal, etc...
-- all have a --set, --force, etc... common arguments. If we went for the ADT we'd end up
-- with SetCompileHLSOption, SetCompileGHCOption, SetInstallCabalOption, etc...
-- which isn't terrible, but verbose enough to reject it.

-- | A newtype for labeling resources in menus. It is bundled along with pattern synonyms
newtype ResourceId = ResourceId Int deriving (Eq, Ord, Show)

pattern OkButton = ResourceId 0
pattern AdvanceInstallButton = ResourceId 100
pattern CompilieButton = ResourceId 101


-- | Name data type. Uniquely identifies each widget in the TUI. 
-- some constructors might end up unused, but still is a good practise
-- to have all of them defined, just in case
data Name = AllTools        -- ^ The main list widget
          | Singular Tool   -- ^ The particular list for each tool
          | KeyInfoBox      -- ^ The text box widget with action informacion
          | TutorialBox     -- ^ The tutorial widget
          | ContextBox      -- ^ The resource for the context menu
          | MenuElement ResourceId  -- ^ The resource for field/buttons in a menu
          deriving (Eq, Ord, Show)

-- | Mode type. It helps to dispatch events to different handlers.
data Mode = Navigation | KeyInfo | Tutorial deriving (Eq, Show, Ord)

installedSign :: String
#if IS_WINDOWS
installedSign = "I "
#else
installedSign = "✓ "
#endif

setSign :: String
#if IS_WINDOWS
setSign = "IS"
#else
setSign = "✔✔"
#endif

notInstalledSign :: String
#if IS_WINDOWS
notInstalledSign = "X "
#else
notInstalledSign = "✗ "
#endif

showKey :: Vty.Key -> String
showKey (Vty.KChar c) = [c]
showKey Vty.KUp = "↑"
showKey Vty.KDown = "↓"
showKey key = tail (show key)

showMod :: Vty.Modifier -> String
showMod = tail . show

-- | Given a KeyComb, produces a string widget with and user friendly text
keyToWidget :: KeyCombination -> Brick.Widget n
keyToWidget (KeyCombination key mods) = Brick.str $ intercalate "+" (showKey key : (showMod <$> mods))

-- | A section separator with max width. Looks like this:    -------- o --------
separator :: Brick.Widget n
separator = Border.hBorder <+> Brick.str " o " <+> Border.hBorder

-- | Used to create a layer on top of the main navigation widget (tutorial, info, menus...)
frontwardLayer :: T.Text -> Brick.Widget n -> Brick.Widget n
frontwardLayer layer_name = 
    Brick.centerLayer 
      . Brick.hLimitPercent 75
      . Brick.vLimitPercent 50
      . Brick.withBorderStyle Border.unicode
      . Border.borderWithLabel (Brick.txt layer_name)

-- I refuse to give this a type signature. 

-- | Given a lens, zoom on it. It is needed because Brick uses microlens but GHCup uses optics.
zoom l = Brick.zoom (toLensVL l)

data BrickData = BrickData
  { _lr    :: [ListResult]
  }
  deriving Show

makeLenses ''BrickData

data BrickSettings = BrickSettings { _showAllVersions :: Bool}
  --deriving Show

makeLenses ''BrickSettings

defaultAppSettings :: BrickSettings
defaultAppSettings = BrickSettings { _showAllVersions = False}
