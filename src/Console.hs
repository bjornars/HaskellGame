module Console where

import System.Console.ANSI
import System.IO

import Control.Lens

import Types

setupTerminal :: IO ()
setupTerminal =  do
    hSetEcho stdin False
    hSetBuffering stdin  NoBuffering
    hSetBuffering stdout NoBuffering
    hideCursor

draw :: World -> IO ()
draw world = do
    let hero = _whero world

    clearScreen
    setCursorPosition (hero^.hxpos) (hero^.hypos)

    setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Blue]
    putStr "@"

resetTerminal :: IO ()
resetTerminal = do
    clearScreen
    setCursorPosition 0 0
    setSGR [Reset]
    showCursor
    putStrLn "Bye"

getInput :: IO Input
getInput = do
    input <- getTermInput
    case input of
        Just i -> return i
        Nothing -> getInput

getTermInput :: IO (Maybe Input)
getTermInput = do
    c <- getChar
    if c == '\27' then getMetaChar else return $ Just $ InputC c

getMetaChar :: IO (Maybe Input)
getMetaChar = do
    moreCharacters <- hReady stdin
    if moreCharacters then do
        c1 <- getChar
        c2 <- getChar
        return $ case [c1, c2] of
            "[A" -> Just $ InputC 'w'
            "[D" -> Just $ InputC 'a'
            "[B" -> Just $ InputC 's'
            "[C" -> Just $ InputC 'd'
            _    -> Nothing
        else return $ Just $ InputS IEscape
