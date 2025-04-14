module NGram
  ( generateNGrams,
    generateSentence,
    saveNGramsToFile,
    normalizeText,
    splitIntoSentences,
    NGram
  ) where
import Data.List (nub)
import qualified Data.Set as Set
import Data.Char (toLower, isAlpha)
import Data.List (tails)
import qualified Data.Map as Map
import System.Random (randomRIO)
import System.IO (writeFile)
import Data.List.Split (splitOn)

type NGram = Map.Map [String] [String] -- храним nграммы

-- Функция для удаления знаков препинания
removePunctuation :: String -> String
removePunctuation = filter (\c -> isAlpha c || c == ' ')

replaceEndPunctuations :: String -> String
replaceEndPunctuations = map replacePunctuation
  where
    replacePunctuation c
      | elem c ".;:)(!?" = '.'
      | otherwise = c

-- Функция для разбивки текста на предложения
splitIntoSentences :: String -> [String]
splitIntoSentences text =
    let sentences = splitOn "." (replaceEndPunctuations text) -- Разделяем текст по точкам
        -- Удаляем пробелы и знаки препинания из каждого предложения
        cleanSentences = map (removePunctuation . filter (/= '\n')) sentences
    in filter (not . null) cleanSentences -- Убираем пустые строки

normalizeText :: String -> [String]
normalizeText = words . map (\c -> if isAlpha c || c == ' ' then toLower c else ' ')

-- Функция для генерации N-грамм из одного предложения
generateNGramsForSentence :: Int -> [String] -> NGram
generateNGramsForSentence n wordsList = Map.fromListWith (++) $ concatMap generateNGramsForOrder [1..n]
  where
    -- Генерация N-грамм для заданного порядка k
    generateNGramsForOrder k =
      [ (take k xs, nub[(unwords (take m (drop k xs)))]) | xs <- tails wordsList, -- возвращает список всех возможных "хвостов" списка wordsLis
                                                          length xs > k, --фильтрует подсписки xs, оставляя только те, которые имеют длину больше k
                                                          m <- [1..(n-k+1)], 
                                                          length (drop k xs) >= m ] -- проверяем, что после удаления первых k элементов из xs остается достаточно элементов, чтобы взять m слов

-- Функция для генерации N-грамм из списка предложений
generateNGrams :: Int -> [String] -> NGram
generateNGrams n sentences = foldr (Map.unionWith (++)) Map.empty ngrams
  where
    ngrams = (map (generateNGramsForSentence n . normalizeText) sentences)
                                                               -- мы берем первые k элементов (take k xs) и добавляем 
                                                                        -- следующее слово ([xs !! k]) в качестве значения для этой n-граммы



generateSentence :: NGram -> [String] -> Int -> IO [String]
generateSentence ngram seed maxLength = do
  
  let initialSentence = seed   --задаёт начальные слова для предложения
  go initialSentence (length seed)
  where
    n = maximum $ map length (Map.keys ngram)
    go currentSentence len = do
      if len >= maxLength
        then return currentSentence
        else do
          let seedLength = min (n - 1) len
              currentSeed = drop (length currentSentence - seedLength) currentSentence -- находим ключ для n-граммы
          nextWord <-getNextWord currentSeed
          case nextWord of
            Just word -> go (currentSentence ++ [firstWordSplit (word)]) (len + 1)
            Nothing   -> return currentSentence

    getNextWord options
      | null options = return Nothing
      | otherwise = case Map.lookup options ngram of -- находим список следующих слов
          Just words -> do
            idx <- randomRIO (0, length words - 1) --ключ найден, выбирается случайное слово из списка продолжений
            return $ Just (words !! idx)
          Nothing -> getNextWord (tail options) --генерация заканчивается

firstWordSplit :: String -> String
firstWordSplit str = (splitOn " " str !!) 0
 
saveNGramsToFile :: FilePath -> NGram -> IO ()
saveNGramsToFile filePath ngrams = do
  let formatEntry (key, values) =
        unwords key ++ " -> " ++ unwords (map (\v -> "'" ++ v ++ "'") (Set.toList $ Set.fromList values))  -- Удаляем дубликаты и добавляем кавычки
      formattedNGrams = unlines $ map formatEntry (Map.toList ngrams)  -- Конвертируем в список строк
  writeFile filePath formattedNGrams  -- Записываем в файл
