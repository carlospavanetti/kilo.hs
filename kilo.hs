module Main where

{-- imports --}

import Data.Bits ((.&.))
import Data.Char (chr, ord, isControl)
import Control.Monad (when)
import Control.Exception (finally, catch, IOException)

import Foreign.C.Error (eAGAIN, getErrno)
import System.IO.Unsafe (unsafePerformIO)
import System.Exit (die, exitSuccess)
import System.Posix.IO
import System.Posix.Terminal

{-- terminal --}

enableRawMode :: IO TerminalAttributes
enableRawMode = do
    current <- getTerminalAttributes stdInput
    let newAttributes =
            flip withoutMode EnableEcho $
            flip withoutMode ProcessInput $
            flip withoutMode KeyboardInterrupts $
            flip withoutMode StartStopOutput $
            flip withoutMode ExtendedFunctions $
            flip withoutMode MapCRtoLF $
            flip withoutMode ProcessOutput $
            flip withoutMode InterruptOnBreak $
            flip withoutMode CheckParity $
            flip withoutMode StripHighBit $
            flip withBits 8 $
            flip withMinInput 0 $
            flip withTime 1 $
            current
    setTerminalAttributes stdInput newAttributes WhenFlushed
    return current

disableRawMode :: TerminalAttributes -> IO ()
disableRawMode attrs = setTerminalAttributes stdInput attrs WhenFlushed

editorReadKey :: IO Char
editorReadKey = do
    (char:[], nread) <- fdRead stdInput 1
    if (nread == -1) && (unsafePerformIO getErrno == eAGAIN)
        then die("read")
        else return char

editorProcessKeypress :: Char -> IO ()
editorProcessKeypress c
        | c == controlKeyMask 'q' = exitSuccess
        | otherwise = return ()

{-- input --}

controlKeyMask = chr . ((.&.) 0x1F) . ord

{-- init --}

main :: IO ()
main = do
    originalAttributes <- enableRawMode
    safeLoop `finally` disableRawMode originalAttributes
    where
        safeLoop = loop `catch` (const $ safeLoop :: IOException -> IO ())
        loop = editorReadKey >>= editorProcessKeypress >> loop
