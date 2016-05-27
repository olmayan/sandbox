-- | File Manager demo.
--  Author      :  Andy Stewart
--  Copyright   :  (c) 2010 Andy Stewart <lazycat.manatee@gmail.com>

-- | This simple file-manager base on gio.
--
module Main where

import Graphics.UI.Gtk
import MainWindow

-- | Main.
main :: IO ()
main = do
    -- Init.
    initGUI

    -- Create window.
    window <- mainWindowNew
    widgetShowAll window

    mainGUI
