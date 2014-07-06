{-# LANGUAGE TemplateHaskell, Rank2Types, FlexibleContexts #-}
module UI where

import Control.Lens
import World
import WorldGenerating

import Control.Monad.State

type UI = StateT UiState IO 

data UiState = UiState {
    _uiWorld :: World
    }

makeLenses ''UiState

startUi :: UI () -> IO ()
startUi ui = do
    runStateT ui simpleUiState
    return ()

simpleUiState :: UiState
simpleUiState = UiState {
    _uiWorld = simpleWorld
    }
