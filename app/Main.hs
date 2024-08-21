{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Main where

import Data.Foldable (traverse_)
import Options.Applicative
import Data.List.Split (splitOn)
import Data.Char (toLower, isSpace)
import System.Console.Haskeline (InputT, runInputT, defaultSettings, getInputLine, outputStrLn)

type Repl a = InputT IO a

data LearningItem = LearningItem
  {
      left :: String
    , right :: String
  }

data CmdOptions = CmdOptions
  {
      trainingSetPath :: String
    , times :: Int
  }

cmdOptionsParser :: Parser CmdOptions
cmdOptionsParser = CmdOptions
  <$> strOption
      ( long "file"
     <> short 'f'
     <> metavar "FILE_PATH"
     <> help "Path to training file" )
  <*> option auto
      ( long "rounds"
     <> short 'n'
     <> metavar "INTEGER"
     <> help "Number of rounds for the set" )

nTimes :: Int -> [a] -> [a]
nTimes n l = concat $ replicate n l

main :: IO ()
main = do
  options <- execParser opts
  items <- readItemsFromFile (trainingSetPath options) "##"
  putStrLn $ "File: " ++ trainingSetPath options
  putStrLn $ "Rounds: " ++ show (times options) <> " Items: " <> (show $ length $ items)
  doTraining $ nTimes (times options) items
  where
    opts = info (cmdOptionsParser <**> helper)
      ( fullDesc
     <> progDesc "HISS2 - TUI for learning languages")

doTraining :: [LearningItem] -> IO ()
doTraining items =
  runInputT defaultSettings $ traverse_ readAndCheck items

leftPad :: Int -> String -> String
leftPad n s = (replicate n ' ') ++ s

printError :: Int -> String -> String -> Repl ()
printError offset expected _actual = outputStrLn $ leftPad offset expected

propmt :: LearningItem -> String
propmt item = right item ++ " > "

isCharInString :: Char -> String -> Bool
isCharInString _ [] = False -- base case: empty string, character not found
isCharInString c (x : xs) -- recursive case: check first character of string
  | c == x = True -- character found
  | otherwise = isCharInString c xs -- character not found yet, check rest of string

stripSpacesToLowerCase :: String -> String
stripSpacesToLowerCase = map toLower . filter (not . (\c -> isSpace c || isCharInString c "!¡.,?¿:;-–'\"()"))

readAndCheck :: LearningItem  -> Repl Bool
readAndCheck item = do
  minput <- getInputLine $ right item ++ "~ "
  case minput of
    Nothing -> return False
    Just input ->
      let result = stripSpacesToLowerCase input == (stripSpacesToLowerCase $ left item) in do
        if not result then printError (length (right item) + 2) (left item) input else return ()
        return result

shuffleLearningItems :: [LearningItem] -> IO [LearningItem]
shuffleLearningItems items = return items

readItemsFromFile :: String -> String -> IO [LearningItem]
readItemsFromFile filePath separator = do
  fileContents <- readFile filePath
  return $ map parseLine $ lines fileContents
  where
    parseLine :: String -> LearningItem
    parseLine line = LearningItem (head parts) (last parts)
      where
        parts = splitOn separator line