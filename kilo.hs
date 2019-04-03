module Main where

import Control.Monad (when)
import System.Posix.IO
import System.Posix.Terminal

enableRawMode :: IO ()
enableRawMode = do
    current <- getTerminalAttributes stdInput
    let newAttributes = withoutMode current EnableEcho
    setTerminalAttributes stdInput newAttributes WhenFlushed

main :: IO()
main = do
    enableRawMode
    (char, count) <- fdRead stdInput 1
    when (count == 1 && char /= "q" ) main