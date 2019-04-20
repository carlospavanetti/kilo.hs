{-# LANGUAGE OverloadedStrings #-}
module Main where

{-- imports --}

import Data.Bits ((.&.))
import Data.Char (chr, ord, isNumber)
import Control.Exception (finally, catch, IOException)
import Control.Monad (void)

import Text.Printf (printf)
import qualified Data.Text as T

import System.IO
import System.Environment

import Foreign.C.Error (eAGAIN, getErrno)
import System.IO.Unsafe (unsafePerformIO)
import System.Exit (die, exitSuccess)
import System.Posix.IO (fdRead, fdWrite, stdInput, stdOutput)
import System.Posix.Terminal

{-- defines --}

kiloVersion :: AppendBuffer
kiloVersion = "0.0.1"

welcomeMessage :: AppendBuffer
welcomeMessage = "Kilo.hs editor -- version " `mappend` kiloVersion

{-- data --}

type Erow = T.Text

data EditorConfig = EditorConfig
    { cursor     :: (Int, Int)
    , rowOffset :: Int
    , colOffset :: Int
    , windowSize :: (Int, Int)
    , numRows :: Int
    , row :: [Erow]
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

escape :: String -> String
escape cmd = "\ESC[" `mappend` cmd

unescape :: String -> Maybe String
unescape ('\ESC': '[': cmd) = Just cmd
unescape _ = Nothing

terminalCommand :: String -> IO ()
terminalCommand cmd = void $ fdWrite stdOutput (escape cmd)

editorPositionCursor :: (Int, Int) -> IO ()
editorPositionCursor (x, y) = terminalCommand positionCursor
  where positionCursor = printf "%d;%dH" y x

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
    readCursorReport :: IO (Maybe String)
    readCursorReport = unescape <$> getUntilR ""
    getUntilR :: String -> IO String
    getUntilR acc =
        editorReadRawChar
        >>= \char -> case char of
            'R' -> return acc
            _   -> getUntilR (acc ++ [char])
    parseReport :: Maybe String -> IO (Int, Int)
    parseReport Nothing = die "getCursorPosition"
    parseReport (Just report) = do
        let (rows, _: cols) = break (== ';') report
        return (read rows, read cols)

getWindowSize :: IO (Int, Int)
getWindowSize = moveToLimit >> getCursorPosition
  where moveToLimit = terminalCommand "999C" >> terminalCommand "999B"

{-- row operations --}

-- editorAppendRow :: EditorConfig -> AppendBuffer -> EditorConfig
-- editorAppendRow config line = config { row = [line], numRows = 1 }

{-- file i/o --}

editorOpen :: FilePath -> EditorConfig -> IO EditorConfig
editorOpen fileName config = do
    file <- openFile fileName ReadMode
    content <- hGetContents file
    let rows = T.splitOn "\n" (T.pack content)
    return config { row =  rows, numRows = length rows }

{-- append buffer --}

type AppendBuffer = T.Text

{-- output --}

editorScroll :: EditorConfig -> EditorConfig
editorScroll config@EditorConfig
    { cursor = (x, y)
    , rowOffset = rowOffset
    , colOffset = colOffset
    , windowSize = (rows, cols)
    } = config { rowOffset = rowOffset', colOffset = colOffset' }
  where
    rowOffset'
        | y <= rowOffset       = y - 1
        | y > rowOffset + rows = y - rows
        | otherwise            = rowOffset
    colOffset'
        | x <= colOffset       = x - 1
        | x > colOffset + cols = x - cols
        | otherwise            = colOffset

clearLineCommand :: AppendBuffer
clearLineCommand = T.pack $ escape "K"

editorRow :: EditorConfig -> Int -> AppendBuffer
editorRow config@EditorConfig
    { windowSize = (windowRows, windowCols)
    , rowOffset = rowOffset
    , colOffset = colOffset
    , numRows = numRows
    , row = row
    , cursor = (_, y)
    } n
    | fileRow <= numRows    = clear . truncate . T.drop colOffset $ currentRow
    | displayWelcomeMessage = clear $ padding `mappend` truncate welcomeMessage
    | otherwise             = clear tilde
  where
    fileRow = n + rowOffset
    currentRow = row !! (fileRow - 1)
    tilde = "~"
    clear row = row `mappend` clearLineCommand `mappend` maybeCRLN
    maybeCRLN = if n == windowRows then "" else "\r\n"
    truncate = T.take windowCols
    displayWelcomeMessage = numRows == 0 && n == windowRows `div` 3
    padding
        | (paddingSize <= 0) = ""
        | otherwise          = tilde `mappend` spaces
    paddingSize = (windowCols - T.length welcomeMessage) `div` 2
    spaces = T.replicate (paddingSize - T.length tilde) " "


editorDrawRows :: EditorConfig -> AppendBuffer
editorDrawRows config = T.concat $ map (editorRow config) [1.. rows]
  where (rows, _) = windowSize config

editorRefreshScreen :: EditorConfig -> IO ()
editorRefreshScreen config@EditorConfig
    { cursor = (x, y)
    , rowOffset = rowOffset
    , colOffset = colOffset
    , windowSize = (rows, cols) } =
    editorHideCursor
    >> editorRepositionCursor
    >> fdWrite stdOutput (T.unpack $ editorDrawRows config)
    >> editorPositionCursor (x - colOffset, y - rowOffset)
    >> editorShowCursor

{-- input --}

controlKey :: Char -> EditorKey
controlKey = Key . chr . ((.&.) 0x1F) . ord

editorMoveCursor :: EditorKey -> EditorConfig -> EditorConfig
editorMoveCursor move config@EditorConfig
    { cursor = cursor@(cx, cy)
    , numRows = numRows
    , row = row
    , windowSize = (rows, cols) } =
    case move of
        ArrowLeft  -> config { cursor = moveByDx (-1) cursor }
        ArrowRight -> config { cursor = moveByDx   1  cursor }
        ArrowUp    -> config { cursor = moveByDy (-1) cursor }
        ArrowDown  -> config { cursor = moveByDy   1  cursor }
        PageUp     -> config { cursor = (cx,    1) }
        PageDown   -> config { cursor = (cx, rows) }
        HomeKey    -> config { cursor = (1,    cy) }
        EndKey     -> config { cursor = (cols, cy) }
        _          -> config
  where
    moveByDy dy (x, y) = boundToWidth $ boundToHeight (x, y + dy)
    moveByDx dx (x, y) = boundToWidth $ changeLine    (x + dx, y)
      where
        previousLine     = y - 1
        (_, currentLine) = boundToHeight (0, y)
        (_, nextLine)    = boundToHeight (0, 1 + currentLine)
        outOfCurrentLine = 1 + endOf currentLine
        changeLine (x, y)
            | x == 0 && y /= 1      = (endOf previousLine, previousLine)
            | x == outOfCurrentLine = (1, nextLine)
            | otherwise             = (x, currentLine)
    boundTo lower higher = max lower . min higher
    boundToHeight (x, y) = (x, boundTo 1 (1 + numRows) y)
    boundToWidth  (x, y) = (boundTo 1 (endOf y) x, y)
    endOf line
        | line > numRows = 0
        | otherwise      = 1 + T.length (row !! (line - 1))

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
    , rowOffset = 0
    , colOffset = 0
    , windowSize = windowSize
    , numRows = 0
    , row = []
    }

main :: IO ()
main = do
    originalAttributes <- enableRawMode
    windowSize <- getWindowSize
    args <- getArgs
    let fileName = head args -- doesn't break thanks to lazy evaluation
    let initConfig = initEditorConfig windowSize
    editorConfig <- if null args
        then return initConfig
        else editorOpen fileName initConfig
    safeLoop editorConfig `finally` disableRawMode originalAttributes
  where
    safeLoop editorConfig = loop editorConfig
        `catch` retry editorConfig
    retry :: EditorConfig -> IOException -> IO ()
    retry = const . safeLoop
    loop editorConfig =
        let scrolledConfig = editorScroll editorConfig in
        editorRefreshScreen scrolledConfig
        >> editorReadKey >>= editorProcessKeypress scrolledConfig
        >>= loop
