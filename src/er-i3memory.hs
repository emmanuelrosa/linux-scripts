{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Main
Description : An i3blocks block which displays the amount of available RAM.
Copyright   : (c) 2019 Emmanuel Rosa
License     : MIT
Maintainer  : emmanuelrosa@protonmail.com
Stability   : experimental
Portability : non-portable (uses Linux system calls)
-}

import Prelude hiding (putStr)
import System.SysInfo
import Formatting.Formatters
import Formatting
import Foreign.C.Types
import Data.Text.IO
import Data.Text.Lazy

{- Note: used RAM is calculated by total - free; does not take cache and buffers into account 
-}
used :: SysInfo -> CULong
#include "usedram.hs"

main :: IO ()
main = do
    info <- sysInfo

    putStr $ toStrict $ either (const "---") format' $ fmap used info

format' = format (bytes (fixed 2 % " "))
