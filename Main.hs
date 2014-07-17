{-# LANGUAGE CPP #-}
module Main where

import UI
import CliUI
#ifdef OPENGL
import GLUI
#endif
#ifdef NCURSES
import NCursesUI
#endif

main :: IO ()
#ifdef OPENGL
main = newGLUI
#elif NCURSES
main = newNCursesUI
#else
main = newCliUI
#endif
