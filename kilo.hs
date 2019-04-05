module Main where

{-- imports --}

import Data.Bits ((.&.))
import Data.Char (chr, ord)
import Control.Exception (finally, catch, IOException)
import Control.Monad (void)

import Foreign.C.Error (eAGAIN, getErrno)
import System.IO.Unsafe (unsafePerformIO)
import System.Exit (die, exitSuccess)
import System.Posix.IO (fdRead, fdWrite, stdInput, stdOutput)
import System.Posix.Terminal

{-- terminal --}

enableRawMode :: IO TerminalAttributes
enableRawMode = getTerminalAttributes stdInput
    >>= \originalAttributes ->
        setStdIOAttributes (withoutCanonicalMode originalAttributes)
        >> return originalAttributes

disableRawMode :: TerminalAttributes -> IO ()
disableRawMode = setStdIOAttributes

setStdIOAttributes :: TerminalAttributes -> IO ()
setStdIOAttributes = flip (setTerminalAttributes stdInput) WhenFlushed

withoutCanonicalMode :: TerminalAttributes -> TerminalAttributes
withoutCanonicalMode = disableModes . set8BitsPerByte . setTimeout
    where
    set8BitsPerByte = flip withBits 8
    setTimeout = (flip withMinInput 0) . (flip withTime 1)
    disableModes = compose $ (flip withoutMode) <$> modesToDisable
    compose = foldl (.) id
    modesToDisable = [
            EnableEcho, ProcessInput, KeyboardInterrupts,
            StartStopOutput, ExtendedFunctions, MapCRtoLF,
            ProcessOutput, InterruptOnBreak, CheckParity,
            StripHighBit
        ]

editorReadKey :: IO Char
editorReadKey = let
    unsafeReadKey = fdRead stdInput 1 >>= handleError
    handleError (char:[], nread) =
        if (nread == -1) 
            then if (unsafePerformIO getErrno /= eAGAIN)
                then die("read")
                else editorReadKey
            else return char
    in unsafeReadKey `catch` (const $ editorReadKey :: IOException -> IO Char)

getCursorPosition :: IO (Int, Int)
getCursorPosition = let
    cursorPositionReportCmd = "\x1B[6n"
    readUntilR acc = do
        char <- editorReadKey
        if char == 'R'
            then return acc
            else readUntilR (acc ++ [char])
    parsePosition ('\x1b': '[': xs) = do
        let (rows, (_:cols)) = break (== ';') xs
        fdWrite stdOutput "\r"
        return (read rows :: Int, read cols :: Int)
    parsePosition _ = die "getCursorPosition"
    in fdWrite stdOutput cursorPositionReportCmd
        >> readUntilR "" >>= parsePosition

getWindowSize :: IO (Int, Int)
getWindowSize = let moveToBottomRightCmd = "\x1b[999C\x1b[999B"
    in fdWrite stdOutput moveToBottomRightCmd >> getCursorPosition

{-- append buffer --}

type AppendBuffer = String

{-- output --}

editorDrawRows :: Int -> AppendBuffer
editorDrawRows 0  = ""
editorDrawRows 1  = "~"
editorDrawRows n  = "~\r\n" ++ (editorDrawRows $ n - 1)

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
    >> getWindowSize
    >>= (fdWrite stdOutput) . editorDrawRows . fst
    >> editorRepositionCursor

{-- input --}

controlKeyMask :: Char -> Char
controlKeyMask = chr . ((.&.) 0x1F) . ord

editorProcessKeypress :: Char -> IO ()
editorProcessKeypress c
        | c == controlKeyMask 'q' = editorClearScreen >> exitSuccess
        | otherwise = return ()

{-- init --}

main :: IO ()
main = do
    originalAttributes <- enableRawMode
    (getWindowSize >>= \x -> putStrLn ('\r': show x) >> editorReadKey
        >> safeLoop) `finally` disableRawMode originalAttributes
    where
    safeLoop = loop `catch` (const $ safeLoop :: IOException -> IO ())
    loop = editorRefreshScreen
            >> editorReadKey >>= editorProcessKeypress
            >> loop
