{-# LANGUAGE OverloadedStrings #-}
module Main where

import Options.Applicative
import Data.List.Split (splitOn)
import System.Console.Haskeline (InputT, runInputT, defaultSettings, getInputLine, outputStrLn)

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
      ( long "training-set"
     <> short 't'
     <> metavar "TRAINING_SET"
     <> help "Path to training set" )
  <*> option auto
      ( long "times"
     <> short 'n'
     <> metavar "TIMES"
     <> help "Number of times to run the training set" )

main :: IO ()
main = do
  options <- execParser opts
  putStrLn $ "Training set path: " ++ trainingSetPath options
  putStrLn $ "Times: " ++ show (times options)
  items <- readItemsFromFile (trainingSetPath options) "##"
  putStrLn $ show $ length $ items
  doTraining items
  where
    opts = info (cmdOptionsParser <**> helper)
      ( fullDesc
     <> progDesc "Run the training set"
     <> header "run-training-set - a program to run the training set" )

type Repl a = InputT IO a

doTraining :: [LearningItem] -> IO ()
doTraining items = do
  runInputT defaultSettings $ loop items
  where
    loop :: [LearningItem] -> Repl ()
    loop items = do
      case items of
        [] -> return ()
        (item: rest) -> do
          _result <- readAndCheck item
          loop rest

printError :: String -> String -> Repl ()
printError _expected actual = outputStrLn $ "Error: " ++ actual

propmt :: LearningItem -> String
propmt item = right item ++ " > "

readAndCheck :: LearningItem  -> Repl Bool
readAndCheck item = do
  minput <- getInputLine $ right item ++ "~ "
  case minput of
    Nothing -> return False
    Just input ->
      let result = input == left item in do
        if not result then printError (left item) input else return ()
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