{-# LANGUAGE OverloadedStrings #-}
module Main where

{-- imports --}

import Data.Bits ((.&.))
import Data.Char (chr, ord, isNumber)
import Data.String (IsString)
import Data.Maybe (fromMaybe)
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

kiloTabStop :: Int
kiloTabStop = 8

{-- data --}

data Erow = Erow { chars :: T.Text, render :: T.Text }

data EditorConfig = EditorConfig
    { cursor     :: (Int, Int)
    , rowOffset  :: Int
    , colOffset  :: Int
    , windowSize :: (Int, Int)
    , numRows    :: Int
    , row        :: [Erow]
    , fileName   :: Maybe String
    }

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
        (seq, nread) <- fdRead stdInput 1
        case nread of
            1 -> select (head seq)
            _ -> return Escape
        `catch` ((\e -> return Escape) :: IOException -> IO EscapeSequence)
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

escape :: (IsString a, Monoid a) => a -> a
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

editorUpdateRow :: T.Text -> Erow
editorUpdateRow row = Erow
    { chars = row
    , render = T.unfoldr transform (1, T.unpack row)
    }
  where
    transform :: (Int, String) -> Maybe (Char, (Int, String))
    transform (n, '\t':cs)
        | reachStop n     = Just (' ', (1, cs))
        | otherwise       = Just (' ', (n + 1, '\t':cs))
    transform (idx, c:cs) = Just (c, (idx + 1, cs))
    transform (idx, "")   = Nothing
    reachStop n = n `mod` kiloTabStop == 0

{-- file i/o --}

editorOpen :: FilePath -> EditorConfig -> IO EditorConfig
editorOpen fileName config = do
    file <- openFile fileName ReadMode
    content <- hGetContents file
    let rows  = T.splitOn "\n" (T.pack content)
    let erows = map editorUpdateRow rows
    return config
        { row = erows, numRows = length rows, fileName = Just fileName }

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
editorRow EditorConfig
    { windowSize = (windowRows, windowCols)
    , rowOffset = rowOffset
    , colOffset = colOffset
    , numRows = numRows
    , row = row
    } rowIndex
    | fileRow <= numRows    = clear . truncate . T.drop colOffset $ currentRow
    | displayWelcomeMessage = clear $ padding `mappend` truncate welcomeMessage
    | otherwise             = clear tilde
  where
    fileRow = rowIndex + rowOffset
    currentRow = render $ row !! (fileRow - 1)
    tilde = "~"
    clear row = row `mappend` clearLineCommand `mappend` maybeCRLN
    maybeCRLN = "\r\n"
    truncate = T.take windowCols
    displayWelcomeMessage = numRows == 0 && rowIndex == windowRows `div` 3
    padding
        | (paddingSize <= 0) = ""
        | otherwise          = tilde `mappend` spaces
    paddingSize = (windowCols - T.length welcomeMessage) `div` 2
    spaces = T.replicate (paddingSize - T.length tilde) " "


editorDrawRows :: EditorConfig -> AppendBuffer
editorDrawRows config = T.concat $ map (editorRow config) [1.. rows]
  where (rows, _) = windowSize config

editorDrawStatusBar :: EditorConfig -> AppendBuffer
editorDrawStatusBar EditorConfig
    { windowSize = (_, cols)
    , fileName = fileName
    , cursor = (_, cy)
    , numRows = numRows
    } = invertCommand `mappend` statusBar `mappend` restoreCommand
  where
    invertCommand  = escape "7m"
    restoreCommand = escape "m"
    name  = fromMaybe "[No Name]" fileName
    fill  = T.replicate (cols - T.length left - T.length right) " "
    left  = T.pack $ printf "%.20s - %d lines" name numRows
    right = T.pack $ printf "%d/%d" (cy - 1) numRows
    statusBar = left `mappend` fill `mappend` right

editorRefreshScreen :: EditorConfig -> IO EditorConfig
editorRefreshScreen config@EditorConfig
    { cursor = (x, y)
    , rowOffset = rowOffset
    , colOffset = colOffset
    , windowSize = (rows, cols) } =
    editorHideCursor
    >> editorRepositionCursor
    >> fdWrite stdOutput (T.unpack $ editorDrawRows config)
    >> fdWrite stdOutput (T.unpack $ editorDrawStatusBar config)
    >> editorPositionCursor (x - colOffset, y - rowOffset)
    >> editorShowCursor
    >> return config

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
        PageUp     -> config { cursor = moveByDy (1 - rows) cursor }
        PageDown   -> config { cursor = moveByDy (rows - 1) cursor }
        HomeKey    -> config { cursor = (1,        cy) }
        EndKey     -> config { cursor = (endOf cy, cy) }
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
        | otherwise      = 1 + T.length (render $ row !! (line - 1))

editorProcessKeypress :: EditorConfig -> IO EditorConfig
editorProcessKeypress config = editorReadKey >>= handleKeypress
  where
    handleKeypress key
        | (key == controlKey 'q') =
            editorClearScreen >> exitSuccess >> return config
        | otherwise = return (editorMoveCursor key config)

{-- init --}

initEditorConfig :: (Int, Int) -> EditorConfig
initEditorConfig (rows, cols) = EditorConfig
    { cursor = (1, 1)
    , rowOffset = 0
    , colOffset = 0
    , windowSize = (rows - 1, cols)
    , numRows = 0
    , row = []
    , fileName = Nothing
    }

firstEditorConfig :: IO EditorConfig
firstEditorConfig = do
    args <- getArgs
    windowSize <- getWindowSize
    let fileName = head args -- doesn't break thanks to lazy evaluation
    let initConfig = initEditorConfig windowSize
    if null args
        then return initConfig
        else editorOpen fileName initConfig

main :: IO ()
main = do
    originalAttributes <- enableRawMode
    editorConfig <- firstEditorConfig
    safeLoop editorConfig `finally` disableRawMode originalAttributes
  where
    safeLoop editorConfig = loop editorConfig
        `catch` retry editorConfig
    retry :: EditorConfig -> IOException -> IO ()
    retry = const . safeLoop
    loop editorConfig =
        editorRefreshScreen (editorScroll editorConfig)
        >>= editorProcessKeypress
        >>= loop
