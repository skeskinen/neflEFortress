module VtyUI where

import System.Exit ( exitSuccess )
import Graphics.Vty
import Graphics.Vty.Widgets.All
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.State

import World
import UI

io :: IO a -> UI a
io = liftIO

newVtyUi :: UI ()
newVtyUi = do
    pb <- io $ editWidget
    ui <- io $ centered pb
    fg <- io $ newFocusGroup
    io $ addToFocusGroup fg pb
    io $ fg `onKeyPressed` \_ k _ -> do
          case k of
            KEsc -> exitSuccess
            _ -> return False
    c <- io $ newCollection
    io $ addToCollection c ui fg
    io $ runUi c $ defaultContext

