module Main where

import Data.Char (ord, isControl)
import Control.Monad (when)
import System.Posix.IO
import System.Posix.Terminal

enableRawMode :: IO TerminalAttributes
enableRawMode = do
    current <- getTerminalAttributes stdInput
    let newAttributes =
            flip withoutMode EnableEcho $
            flip withoutMode ProcessInput $
            flip withoutMode KeyboardInterrupts $
            current
    setTerminalAttributes stdInput newAttributes WhenFlushed
    return current

disableRawMode :: TerminalAttributes -> IO ()
disableRawMode attrs = setTerminalAttributes stdInput attrs WhenFlushed

main :: IO()
main = do
    originalAttributes <- enableRawMode
    loop
    disableRawMode originalAttributes
    where loop = do
            (char:[], count) <- fdRead stdInput 1
            if isControl char
                then putStrLn $ show $ ord char
                else putStrLn $ (show $ ord char) ++ " (" ++ char:[] ++ ")"
            when (count == 1 && char /= 'q' ) loop