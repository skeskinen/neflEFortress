{-# LANGUAGE TemplateHaskell #-}
module AnsiUI where

import System.Console.ANSI
import Data.Lens
import Control.Monad.State

import UI

type AnsiUI = StateT AnsiUIState UI

data AnsiView = MainMenu | GameView

data AnsiUIState = AnsiUIState {
    _ansiView :: AnsiView
}

makeLenses ''AnsiUIState

newAnsiUI = return ()
