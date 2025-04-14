module Main where

import NGram (normalizeText, generateNGrams, generateSentence, saveNGramsToFile, splitIntoSentences)
import Dialog (startDialog)
import Lib (checkStr)
import System.Random (randomRIO)

import System.IO

main :: IO ()
main = do
  putStrLn "Choose mode:"
  putStrLn "1. Generate sentence"
  putStrLn "2. Save N-grams to file"
  putStrLn "3. Start dialog between two models"
  putStrLn "4. Run Lib.hs task (Binary Comparison Checker)"
  mode <- getLine
  case mode of
    "1" -> generateSentenceMode
    "2" -> saveNGramsMode
    "3" -> startDialogMode
    "4" -> libTaskMode
    _   -> putStrLn "Invalid option, exiting."

generateSentenceMode :: IO ()
generateSentenceMode = do
  putStrLn "Enter the filename for N-gram generation:"
  textFile <- getLine
  textContent <- readFile textFile
  let wordsList = splitIntoSentences textContent
      n = 3
      ngrams = generateNGrams n wordsList
  putStrLn "Enter starting words for sentence generation (separated by spaces):"
  startWords <- getLine
  let seed = words startWords
  curLen <- randomRIO (2, 15) 
  sentence <- generateSentence ngrams seed curLen
  putStrLn "\nGenerated sentence:"
  putStrLn $ unwords sentence

saveNGramsMode :: IO ()
saveNGramsMode = do
  putStrLn "Enter the filename for N-gram generation:"
  textFile <- getLine
  putStrLn "Enter the output filename for saving N-grams:"
  outputFile <- getLine
  textContent <- readFile textFile
  let wordsList = splitIntoSentences textContent
      n = 2
      ngrams = generateNGrams n wordsList
  saveNGramsToFile outputFile ngrams
  putStrLn $ "N-grams saved to " ++ outputFile

startDialogMode :: IO ()
startDialogMode = do
  putStrLn "Enter the filename for the first text:"
  textFile1 <- getLine
  textContent1 <- readFile textFile1
  putStrLn "Enter the filename for the second text:"
  textFile2 <- getLine
  textContent2 <- readFile textFile2
  putStrLn "Enter the number of messages in the dialog (M):"
  mStr <- getLine
  let m = read mStr :: Int
      n = 2
  startDialog n m (splitIntoSentences textContent1) (splitIntoSentences textContent2)

libTaskMode :: IO ()
libTaskMode = do
  putStrLn "Enter filename."
  mStr <- getLine
  putStrLn ("Processing binary comparisons from "++ show mStr ++"...")
  content <- readFile mStr
  let linesOfFile = lines content
  mapM_ processLine linesOfFile

processLine :: String -> IO ()
processLine line = do
  let result = checkStr line
  case result of
    Just True  -> putStrLn $ line ++ ": Valid and True"
    Just False -> putStrLn $ line ++ ": Valid and False"
    Nothing    -> putStrLn $ line ++ ": Invalid"