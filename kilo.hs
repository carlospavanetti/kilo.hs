module Main where

import Control.Monad (when)
import System.Posix.IO
import System.Posix.Terminal

enableRawMode :: IO TerminalAttributes
enableRawMode = do
    current <- getTerminalAttributes stdInput
    let newAttributes = withoutMode current EnableEcho
    setTerminalAttributes stdInput newAttributes WhenFlushed
    return current

disableRawMode :: TerminalAttributes -> IO ()
disableRawMode attrs = setTerminalAttributes stdInput attrs WhenFlushed

main :: IO()
main = do
    originalAttributes <- enableRawMode
    (char, count) <- fdRead stdInput 1
    when (count == 1 && char /= "q" ) main
    disableRawMode originalAttributes