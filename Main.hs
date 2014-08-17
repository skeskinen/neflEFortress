{-# LANGUAGE CPP #-}
module Main where

import Ui
#ifdef OPENGL
import GLUi
#endif
#ifdef NCURSES
import NCursesUi
#endif

main :: IO ()
#ifdef OPENGL
main = newGLUi
#elif NCURSES
main = newNCursesUi
#else
main = newCliUi
#endif
