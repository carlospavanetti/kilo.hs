module Main where

{-- imports --}

import Data.Bits ((.&.))
import Data.Char (chr, ord)
import Control.Exception (finally, catch, IOException)
import Control.Monad (void)

import Foreign.C.Error (eAGAIN, getErrno)
import System.IO.Unsafe (unsafePerformIO)
import System.Exit (die, exitSuccess)
import System.Posix.IO
import System.Posix.Terminal

{-- terminal --}

enableRawMode :: IO TerminalAttributes
enableRawMode = do
    originalAttributes <- getTerminalAttributes stdInput
    let newAttributes =
            disableModes . set8BitsPerByte . setTimeout $ originalAttributes
    setTerminalAttributes stdInput newAttributes WhenFlushed
    return originalAttributes
    where
        set8BitsPerByte = flip withBits 8
        setTimeout = (flip withMinInput 0) . (flip withTime 1)
        disableModes = compose $ (flip withoutMode) <$> modesToDisable
        modesToDisable = [
                EnableEcho, ProcessInput, KeyboardInterrupts,
                StartStopOutput, ExtendedFunctions, MapCRtoLF,
                ProcessOutput, InterruptOnBreak, CheckParity,
                StripHighBit
            ]
        compose = foldl (.) id

disableRawMode :: TerminalAttributes -> IO ()
disableRawMode attrs = setTerminalAttributes stdInput attrs WhenFlushed

editorReadKey :: IO Char
editorReadKey = do
    (char:[], nread) <- fdRead stdInput 1
    if (nread == -1) && (unsafePerformIO getErrno == eAGAIN)
        then die("read")
        else return char

{-- output --}

editorDrawRows :: Int -> IO ()
editorDrawRows 0 = return ()
editorDrawRows n =
    fdWrite stdOutput "~\r\n" >> editorDrawRows (n - 1)

editorRepositionCursor :: IO ()
editorRepositionCursor = let repositionCmd = "\x1B[H"
    in void $ fdWrite stdOutput repositionCmd

editorClearScreen :: IO ()
editorClearScreen = let clearCmd = "\x1B[2J"
    in fdWrite stdOutput clearCmd
        >> editorRepositionCursor

editorRefreshScreen :: IO ()
editorRefreshScreen =
    editorClearScreen
    >> editorDrawRows 24
    >> editorRepositionCursor

{-- input --}

controlKeyMask = chr . ((.&.) 0x1F) . ord

editorProcessKeypress :: Char -> IO ()
editorProcessKeypress c
        | c == controlKeyMask 'q' = editorClearScreen >> exitSuccess
        | otherwise = return ()

{-- init --}

main :: IO ()
main = do
    originalAttributes <- enableRawMode
    safeLoop `finally` disableRawMode originalAttributes
    where
        safeLoop = loop `catch` (const $ safeLoop :: IOException -> IO ())
        loop = editorRefreshScreen
                >> editorReadKey >>= editorProcessKeypress
                >> loop
