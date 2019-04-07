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

{-- defines --}

kiloVersion :: String
kiloVersion = "0.0.1"

welcomeMessage = "Kilo.hs editor -- version " ++ kiloVersion

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

editorPositionCursor :: Int -> Int -> IO ()
editorPositionCursor x y =
    let positionCmd = "\x1B[" ++ (show x) ++ ";" ++ (show y) ++ "H"
    in void $ fdWrite stdOutput positionCmd

editorRepositionCursor :: IO ()
editorRepositionCursor = let repositionCmd = "\x1B[H"
    in void $ fdWrite stdOutput repositionCmd

editorHideCursor :: IO ()
editorHideCursor = let hideCursorCmd = "\x1B[?25l"
    in void $ fdWrite stdOutput hideCursorCmd

editorShowCursor :: IO ()
editorShowCursor = let showCursorCmd = "\x1B[?25h"
    in void $ fdWrite stdOutput showCursorCmd

editorClearScreen :: IO ()
editorClearScreen = let clearCmd = "\x1B[2J"
    in fdWrite stdOutput clearCmd
        >> editorRepositionCursor

getCursorPosition :: IO (Int, Int)
getCursorPosition = let
    cursorPositionReportCmd = "\x1B[6n"
    readUntilR acc = do
        char <- editorReadKey
        case char of
            'R' -> return acc
            _   -> readUntilR (acc ++ [char])
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

clearLineCommand :: AppendBuffer
clearLineCommand = "\x1B[K"

editorRow :: Int -> Int -> Int -> AppendBuffer
editorRow windowRows windowCols n
    | n == windowRows `div` 3 = padding ++ welcomeLine ++ "\r\n"
    | n == windowRows = tilde
    | otherwise = tilde ++ "\r\n"
    where
        tilde = '~': clearLineCommand
        welcomeLine = welcomeMessage ++ clearLineCommand
        padding
            | (paddingSize == 0) = ""
            | otherwise = '~': spaces
        paddingSize = min windowCols (
            (windowCols - length welcomeMessage) `div` 2)
        spaces = foldr (:) "" (replicate (paddingSize - 1) ' ')

editorDrawRows :: Int -> Int -> AppendBuffer
editorDrawRows rows cols = foldr1 (++) (map (editorRow rows cols) [1.. rows])

editorRefreshScreen :: Int -> Int -> IO ()
editorRefreshScreen rows cols =
    editorHideCursor
    >> editorRepositionCursor
    >> fdWrite stdOutput (editorDrawRows rows cols)
    >> editorPositionCursor 1 1
    >> editorShowCursor

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
    windowSize <- getWindowSize
    (safeLoop windowSize) `finally` disableRawMode originalAttributes
    where
    safeLoop ws = (loop ws) `catch` (
        const $ safeLoop ws :: IOException -> IO ())
    loop ws = editorRefreshScreen `uncurry` ws
            >> editorReadKey >>= editorProcessKeypress
            >> loop ws
