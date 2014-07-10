module Main where

import UI
import CliUI

main :: IO ()
main = startUI newCliUI
