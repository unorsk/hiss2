{-# LANGUAGE OverloadedStrings #-}

import System.Random.Shuffle (shuffleM)
import System.IO
    ( hFlush, stdout, hSetBuffering, BufferMode(NoBuffering), stdin )
import Data.List.Split (splitOn)
import System.Console.ANSI (setCursorPosition, hideCursor, showCursor, clearFromCursorToScreenEnd, getTerminalSize, setCursorPosition, setSGR, SGR(..), ConsoleLayer(..), ColorIntensity(..), Color(..))
import Control.Exception (bracket_)

data Flashcard = Flashcard { front :: String, back :: [String] }


mainProgram :: IO ()
mainProgram = do
    hSetBuffering stdin NoBuffering
    flashcards <- loadFlashcards "cissp-acronyms.txt"
    shuffledCards <- shuffleM flashcards
    navigateFlashcards shuffledCards 0 False
    
main :: IO ()
main = do
    bracket_ hideCursor showCursor mainProgram


loadFlashcards :: String -> IO [Flashcard]
loadFlashcards filePath = do
  fileContents <- readFile filePath
  return $ map parseLine $ lines fileContents
  where
    separator = "##"
    parseLine :: String -> Flashcard
    parseLine line = Flashcard (head parts) (splitOn "!!" (last parts))
      where
        parts = splitOn separator line

navigateFlashcards :: [Flashcard] -> Int -> Bool -> IO ()
navigateFlashcards cards index showBack = do
    clearScreen
    let card = cards !! index
    let content = if showBack then [front card] ++ back card else [front card]
    -- let displayText = domain card ++ "\n\n" ++ content
    let displayText = content
    let nav = "[h ←] [l →] [Space] [^c] "
    let status = " " ++ show (index + 1) ++ " of " ++ show (length cards) ++ " "
    (termHeight, termWidth) <- getTermSize
    mapM_ (\t -> putStrLn $ centerText termWidth t) displayText
    setCursorPosition termHeight 0
    setSGR [SetColor Background Vivid Green, SetColor Foreground Vivid White]
    putStr $ nav <> (replicate (termWidth - (length nav) - (length status)) ' ')
    setSGR [SetColor Background Vivid Red, SetColor Foreground Vivid White]
    putStr status
    setSGR [Reset]
    hFlush stdout
    c <- getChar
    case c of
        ' ' -> navigateFlashcards cards index (not showBack)
        'h' -> navigateFlashcards cards (max 0 (index - 1)) False
        'l' -> navigateFlashcards cards (min (length cards - 1) (index + 1)) False
        'q' -> return ()
        _   -> navigateFlashcards cards index showBack

getTermSize :: IO (Int, Int)
getTermSize = do
    size <- getTerminalSize
    case size of
        Just termSize -> return termSize
        Nothing -> return (20, 80)

centerText :: Int -> String -> String
centerText termWidth text = unlines $ map centerLine (lines text)
  where
    centerLine line = replicate ((termWidth - length line) `div` 2) ' ' ++ line

clearScreen :: IO ()
clearScreen = do
    setCursorPosition 0 0
    clearFromCursorToScreenEnd