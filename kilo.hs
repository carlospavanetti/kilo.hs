module Main where

{-- imports --}

import Data.Bits ((.&.))
import Data.Char (chr, ord)
import Control.Exception (finally, catch, IOException)
import Control.Monad (void, liftM2)

import Foreign.C.Error (eAGAIN, getErrno)
import System.IO.Unsafe (unsafePerformIO)
import System.Exit (die, exitSuccess)
import System.Posix.IO (fdRead, fdWrite, stdInput, stdOutput)
import System.Posix.Terminal

{-- defines --}

kiloVersion :: AppendBuffer
kiloVersion = "0.0.1"

welcomeMessage :: AppendBuffer
welcomeMessage = "Kilo.hs editor -- version " ++ kiloVersion

{-- data --}

data EditorConfig = EditorConfig
    { cursor :: (Int, Int)
    , windowSize :: (Int, Int) } deriving Show

{-- terminal --}

enableRawMode :: IO TerminalAttributes
enableRawMode =
    getTerminalAttributes stdInput
    -- >>= liftM2 (>>)
    --     (setStdIOAttributes . withoutCanonicalMode)
    --     return
    {-- TODO: A versão com liftM2 não tem semântica alguma --}
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
    setTimeout = flip withMinInput 0 . flip withTime 1
    disableModes = compose $ flip withoutMode <$> modesToDisable
    compose = foldr (.) id
    modesToDisable =
        [   EnableEcho, ProcessInput, KeyboardInterrupts
        ,   StartStopOutput, ExtendedFunctions, MapCRtoLF
        ,   ProcessOutput, InterruptOnBreak, CheckParity
        ,   StripHighBit
        ]

editorReadKey :: IO Char
editorReadKey = let
    unsafeReadKey = fdRead stdInput 1 >>= handleError
    handleError ([char], nread)
        | (nread == -1) = repeatOrDie
        | otherwise = return char
    repeatOrDie
        | (unsafePerformIO getErrno == eAGAIN) = editorReadKey
        | otherwise = die "read"
    in unsafeReadKey
        `catch` (const editorReadKey :: IOException -> IO Char)

editorPositionCursor :: (Int, Int) -> IO ()
editorPositionCursor (x, y) =
    let positionCmd = show y ++ ";" ++ show x ++ "H"
    in writeTerminalCommand positionCmd

{-- TODO: Refatorar o padraozinho abaixo de let in void $ fdWrite --}
editorRepositionCursor :: IO ()
editorRepositionCursor = writeTerminalCommand "H"

editorHideCursor :: IO ()
editorHideCursor = writeTerminalCommand "?25l"

editorShowCursor :: IO ()
editorShowCursor = writeTerminalCommand "?25h"

editorClearScreen :: IO ()
editorClearScreen = let clearCmd = "\x1B[2J"
    in writeTerminalCommand "2J"
        >> editorRepositionCursor

writeTerminalCommand :: AppendBuffer -> IO ()
writeTerminalCommand cmd = void $ fdWrite stdOutput (escape cmd)

escape :: AppendBuffer -> AppendBuffer
escape cmd = '\x1B': '[': cmd

unescape :: AppendBuffer -> Maybe AppendBuffer
unescape ('\x1B': '[': cmd) = Just cmd
unescape _ = Nothing

{-- TODO: Tem que refatorar isso aqui --}
getCursorPosition'' :: IO (Int, Int)
getCursorPosition'' = let
    cursorPositionReportCmd = "\x1B[6n"
    readUntilR acc = do
        char <- editorReadKey
        case char of
            'R' -> return acc
            _   -> readUntilR (acc ++ [char])
    parsePosition ('\x1b': '[': xs) = do
        let (rows, _: cols) = break (== ';') xs
        fdWrite stdOutput "\r"
        return (read rows :: Int, read cols :: Int)
    parsePosition _ = die "getCursorPosition"
    in fdWrite stdOutput cursorPositionReportCmd
        >> readUntilR "" >>= parsePosition

getCursorPosition :: IO (Int, Int)
getCursorPosition =
    writeTerminalCommand "6n"
    >> readCursorReport
    >>= parsePosition
  where
    readCursorReport :: IO (Maybe AppendBuffer)
    -- readCursorReport = readKeysUntilR "" >>= return . unescape
    readCursorReport = unescape <$> readKeysUntilR ""
    readKeysUntilR :: AppendBuffer -> IO AppendBuffer
    readKeysUntilR acc =
        editorReadKey
        >>= \char -> case char of
            'R' -> return acc
            _   -> readKeysUntilR (acc ++ [char])
    parsePosition :: Maybe AppendBuffer -> IO (Int, Int)
    parsePosition (Just report) = do
        let (rows, _: cols) = break (== ';') report
        fdWrite stdOutput "\r"
        return (read rows, read cols)
    parsePosition Nothing = die "getCursorPosition'"

getWindowSize :: IO (Int, Int)
getWindowSize = let moveToBottomRightCmd = "\x1b[999C\x1b[999B"
    in fdWrite stdOutput moveToBottomRightCmd >> getCursorPosition

{-- append buffer --}

type AppendBuffer = String

-- TODO: Trocar os (++) por um (`mappend`) :: Monoid a => a -> a -> a

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
        | paddingSize == 0 = ""
        | otherwise = '~': spaces
    paddingSize = min windowCols (
        (windowCols - length welcomeMessage) `div` 2)
    spaces = foldr (:) "" (replicate (paddingSize - 1) ' ')

editorDrawRows :: Int -> Int -> AppendBuffer
editorDrawRows rows cols = concatMap (editorRow rows cols) [1.. rows]

editorRefreshScreen :: EditorConfig -> IO ()
editorRefreshScreen EditorConfig
    { cursor = cursor
    , windowSize = (rows, cols) } =
        editorHideCursor
        >> editorRepositionCursor
        >> fdWrite stdOutput (editorDrawRows rows cols)
        >> editorPositionCursor cursor
        >> editorShowCursor

{-- input --}

controlKeyMask :: Char -> Char
controlKeyMask = chr . (.&.) 0x1F . ord

editorMoveCursor :: Char -> EditorConfig -> EditorConfig
editorMoveCursor move config@EditorConfig { cursor = (x, y) } =
    case move of
        'a' -> config { cursor = (x - 1, y) }
        'd' -> config { cursor = (x + 1, y) }
        'w' -> config { cursor = (x, y - 1) }
        's' -> config { cursor = (x, y + 1) }

editorProcessKeypress :: EditorConfig -> Char -> IO EditorConfig
editorProcessKeypress config char
    | char == controlKeyMask 'q' =
        editorClearScreen >> exitSuccess >> return config
    | char `elem` "wasd" = return newEditorConfig
    | otherwise = return config
  where
    newEditorConfig :: EditorConfig
    newEditorConfig = editorMoveCursor char config

{-- init --}

initEditorConfig :: (Int, Int) -> EditorConfig
initEditorConfig windowSize = EditorConfig
    { cursor = (1, 1)
    , windowSize = windowSize }

main :: IO ()
main = do
    originalAttributes <- enableRawMode
    windowSize <- getWindowSize
    let config = initEditorConfig windowSize
    safeLoop config `finally` disableRawMode originalAttributes
  where
    safeLoop config = loop config
        `catch` (const $ safeLoop config :: IOException -> IO ())
    loop config =
        editorRefreshScreen config
        >> editorReadKey >>= editorProcessKeypress config
        >>= loop
