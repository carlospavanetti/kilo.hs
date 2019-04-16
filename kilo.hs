module Main where

{-- imports --}

import Data.Bits ((.&.))
import Data.Char (chr, ord, isNumber)
import Control.Exception (finally, catch, IOException)
import Control.Monad (void)

import qualified Data.Text as T

import Foreign.C.Error (eAGAIN, getErrno)
import System.IO.Unsafe (unsafePerformIO)
import System.Exit (die, exitSuccess)
import System.Posix.IO (fdRead, fdWrite, stdInput, stdOutput)
import System.Posix.Terminal

{-- defines --}

kiloVersion :: String
kiloVersion = "0.0.1"

welcomeMessage = "Kilo.hs editor -- version " ++ kiloVersion

{-- data --}

type Erow = T.Text

data EditorConfig = EditorConfig
    { cursor     :: (Int, Int)
    , windowSize :: (Int, Int)
    , numRows :: Int
    , row :: Erow
    } deriving Show

data EditorKey
    = Key Char
    | ArrowLeft | ArrowRight
    | ArrowUp | ArrowDown
    | PageUp | PageDown
    | HomeKey | EndKey | DelKey
    deriving Eq

{-- terminal --}

enableRawMode :: IO TerminalAttributes
enableRawMode =
    getTerminalAttributes stdInput
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
    compose = foldr (.) id
    modesToDisable =
        [   EnableEcho, ProcessInput, KeyboardInterrupts
        ,   StartStopOutput, ExtendedFunctions, MapCRtoLF
        ,   ProcessOutput, InterruptOnBreak, CheckParity
        ,   StripHighBit
        ]

editorReadRawChar :: IO Char
editorReadRawChar = let
    unsafeReadKey = fdRead stdInput 1 >>= handleError
    handleError ([char], nread)
        | (nread == -1) = repeatOrDie
        | otherwise = return char
    repeatOrDie
        | (unsafePerformIO getErrno == eAGAIN) = editorReadRawChar
        | otherwise = die "read"
    retry :: IOException -> IO Char
    retry = const editorReadRawChar
    in unsafeReadKey `catch` retry

editorReadKey :: IO EditorKey
editorReadKey = editorReadRawChar >>= handleEscapeSequence

data EscapeSequence
    = Escape | Introducer
    | TildeCode Char | O_Code
    | Final EditorKey

handleEscapeSequence :: Char -> IO EditorKey
handleEscapeSequence key
    | (key == '\ESC') = processResult <$> (readIntroducer >>= handleCode)
    | otherwise = return (Key key)
  where
    readRawByte :: (Char -> IO EscapeSequence) -> IO EscapeSequence
    readRawByte select = do
        ([seq], nread) <- fdRead stdInput 1
        case nread of
            1 -> select seq
            _ -> return Escape
    readIntroducer :: IO EscapeSequence
    readIntroducer = readRawByte $
        \seq -> if seq == '['
            then return Introducer
            else return Escape
    handleCode :: EscapeSequence -> IO EscapeSequence
    handleCode Escape = return Escape
    handleCode Introducer = handle1st >>= handle2nd
    handle1st ::  IO EscapeSequence
    handle1st = readRawByte $
        \seq -> if isNumber seq
            then return (TildeCode seq)
            else case seq of
                'A' -> return $ Final ArrowUp
                'B' -> return $ Final ArrowDown
                'C' -> return $ Final ArrowRight
                'D' -> return $ Final ArrowLeft
                'H' -> return $ Final HomeKey
                'F' -> return $ Final EndKey
                'O' -> return O_Code
                _   -> return Escape
    handle2nd :: EscapeSequence -> IO EscapeSequence
    handle2nd Escape = return Escape
    handle2nd key@(Final _) = return key
    handle2nd (TildeCode digit) = handleTildeCode digit
    handle2nd O_Code = handleO_Code
    handleTildeCode :: Char -> IO EscapeSequence
    handleTildeCode digit = readRawByte $
        \seq -> if seq /= '~'
            then return Escape
            else case digit of
                '1' -> return $ Final HomeKey
                '3' -> return $ Final DelKey
                '4' -> return $ Final EndKey
                '5' -> return $ Final PageUp
                '6' -> return $ Final PageDown
                '7' -> return $ Final HomeKey
                '8' -> return $ Final EndKey
                _   -> return Escape
    handleO_Code :: IO EscapeSequence
    handleO_Code = readRawByte $ \seq ->
        case seq of
            'H' -> return $ Final HomeKey
            'F' -> return $ Final EndKey
            _   -> return Escape
    processResult :: EscapeSequence -> EditorKey
    processResult Escape = Key '\ESC'
    processResult (Final key) = key

escape :: AppendBuffer -> AppendBuffer
escape cmd = '\ESC': '[': cmd

unescape :: AppendBuffer -> Maybe AppendBuffer
unescape ('\ESC': '[': cmd) = Just cmd
unescape _ = Nothing

terminalCommand :: AppendBuffer -> IO ()
terminalCommand cmd = void $ fdWrite stdOutput (escape cmd)

editorPositionCursor :: (Int, Int) -> IO ()
editorPositionCursor (x, y) =
    let positionCursor = show y ++ ";" ++ show x ++ "H"
    in terminalCommand positionCursor

editorRepositionCursor :: IO ()
editorRepositionCursor = terminalCommand "H"

editorHideCursor :: IO ()
editorHideCursor = terminalCommand "?25l"

editorShowCursor :: IO ()
editorShowCursor = terminalCommand "?25h"

editorClearScreen :: IO ()
editorClearScreen = terminalCommand "2J" >> editorRepositionCursor

getCursorPosition :: IO (Int, Int)
getCursorPosition =
    terminalCommand "6n"
    >> readCursorReport >>= parseReport
  where
    readCursorReport :: IO (Maybe AppendBuffer)
    readCursorReport = unescape <$> getUntilR ""
    getUntilR :: AppendBuffer -> IO AppendBuffer
    getUntilR acc =
        editorReadRawChar
        >>= \char -> case char of
            'R' -> return acc
            _   -> getUntilR (acc ++ [char])
    parseReport :: Maybe AppendBuffer -> IO (Int, Int)
    parseReport Nothing = die "getCursorPosition"
    parseReport (Just report) = do
        let (rows, _: cols) = break (== ';') report
        fdWrite stdOutput "\r"
        return (read rows, read cols)

getWindowSize :: IO (Int, Int)
getWindowSize = moveToLimit >> getCursorPosition
  where moveToLimit = terminalCommand "999C" >> terminalCommand "999B"

{-- afile i/o --}

editorOpen :: EditorConfig -> EditorConfig
editorOpen config = config { row = T.pack "Hello, world!", numRows = 1 }

{-- append buffer --}

type AppendBuffer = String

{-- output --}

clearLineCommand :: AppendBuffer
clearLineCommand = escape "K"

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
    paddingSize = min windowCols $
        (windowCols - length welcomeMessage) `div` 2
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

controlKey :: Char -> EditorKey
controlKey = Key . chr . ((.&.) 0x1F) . ord

editorMoveCursor :: EditorKey -> EditorConfig -> EditorConfig
editorMoveCursor move config@EditorConfig
    { cursor = (x, y)
    , windowSize = (rows, cols) } =
    case move of
        ArrowLeft  -> config { cursor = boundToScreenSize (x - 1, y) }
        ArrowRight -> config { cursor = boundToScreenSize (x + 1, y) }
        ArrowUp    -> config { cursor = boundToScreenSize (x, y - 1) }
        ArrowDown  -> config { cursor = boundToScreenSize (x, y + 1) }
        PageUp     -> config { cursor = (x,    1) }
        PageDown   -> config { cursor = (x, rows) }
        HomeKey    -> config { cursor = (1,    y) }
        EndKey     -> config { cursor = (cols, y) }
        _   -> config
  where
    boundToScreenSize (x, y) = (boundTo 1 cols x, boundTo 1 rows y)
    boundTo lower higher = max lower . min higher

editorProcessKeypress :: EditorConfig -> EditorKey -> IO EditorConfig
editorProcessKeypress config key
    | (key == controlKey 'q') =
        editorClearScreen >> exitSuccess >> return config
    | otherwise = return newEditorConfig
  where
    newEditorConfig :: EditorConfig
    newEditorConfig = editorMoveCursor key config

{-- init --}

initEditorConfig :: (Int, Int) -> EditorConfig
initEditorConfig windowSize = EditorConfig
    { cursor = (1, 1)
    , windowSize = windowSize
    , numRows = 0
    , row = T.pack ""
    }

main :: IO ()
main = do
    originalAttributes <- enableRawMode
    windowSize <- getWindowSize
    let editorConfig = editorOpen $ initEditorConfig windowSize
    safeLoop editorConfig `finally` disableRawMode originalAttributes
  where
    safeLoop editorConfig = loop editorConfig
        `catch` retry editorConfig
    retry :: EditorConfig -> IOException -> IO ()
    retry = const . safeLoop
    loop editorConfig =
        editorRefreshScreen editorConfig
        >> editorReadKey >>= editorProcessKeypress editorConfig
        >>= loop
