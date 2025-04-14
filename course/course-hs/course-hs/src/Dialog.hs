module Dialog
  ( startDialog
  ) where

import NGram (generateNGrams, generateSentence, NGram)
import Lib (checkStr)
import System.Random (randomRIO)
import qualified Data.Map as Map

startDialog :: Int -> Int -> [String] -> [String] -> IO ()
startDialog n m text1 text2 = do
  let ngram1 = generateNGrams n text1
      ngram2 = generateNGrams n text2
  putStrLn $ "Enter up to " ++ show (n) ++ " starting words for the first model (separated by spaces):"
  startWords <- getLine
  let seed1 = take (n) $ words startWords -- берем n словв
  if null seed1  -- если пользователь не ввел слова
    then do
      randomIdx <- randomRIO (0, length (Map.keys ngram1) - 1)
      let randomKey = Map.keys ngram1 !! randomIdx
      dialogLoop n m ngram1 ngram2 randomKey []
    else dialogLoop n m ngram1 ngram2 seed1 []

dialogLoop :: Int -> Int -> NGram -> NGram -> [String] -> [[String]] -> IO ()
dialogLoop _ 0 _ _ _ _ = putStrLn "Dialog finished." -- m=0
dialogLoop n m ngram1 ngram2 currentSeed history = do
  response1 <- generateResponse ngram1 currentSeed history -- историю храним чтобы не циклиться
  putStrLn $ "Model 1: " ++ unwords response1 -- генерируем ответ 1 модели
  if response1 `elem` history
    then putStrLn "Dialog ended to prevent looping."
    else do
      let nextSeed1 = prevWord response1 0 ngram2-- Если ответ не был найден в истории, создается 
                                                               -- новое начало для второй модели, беря последние n - 1 слова из ответа первой моде                                                               
      response2 <- generateResponse ngram2 nextSeed1 (response1 : history)
      putStrLn $ "Model 2: " ++ unwords response2
      let nextSeed2 = prevWord response2 0 ngram1
      dialogLoop n (m - 1) ngram1 ngram2 nextSeed2 (response2 : response1 : history)

findNextSeed :: NGram -> [String] -> Bool
findNextSeed ngram nextSeed =  case Map.lookup nextSeed ngram of
       Just value -> True
       Nothing    -> False

prevWord :: [String] -> Int -> NGram -> [String]
prevWord response1 i ngram 
  | i >= length response1 = [response1 !! (length response1 - 1)] -- Если i больше или равно длине response1, возвращаем пустой список
  | otherwise = 
      let nextSeed1 = response1 !! (length response1 - 1 - i) -- Получаем элемент из response1
      in if findNextSeed ngram [nextSeed1]
         then [nextSeed1] -- Возвращаем список с найденным словом
         else prevWord response1 (i + 1) ngram 

generateResponse :: NGram -> [String] -> [[String]] -> IO [String]
generateResponse ngram seed history = do
  curLen <- randomRIO (2, 15) 
  sentence <- generateSentence ngram seed curLen
  return sentence
