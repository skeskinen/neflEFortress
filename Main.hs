module Main where

import UI
import VtyUI
import CliUI

main :: IO ()
main = startUi newCliUi
