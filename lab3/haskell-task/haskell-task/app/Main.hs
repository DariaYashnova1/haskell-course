-- Файл: Main.hs
module Main (main) where

import Lib
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import System.Exit (exitFailure)
import System.Directory
import System.FilePath
import Data.Char (isSpace)
import Control.Monad (when, unless)
import Data.List.Split (splitOn)

main :: IO ()
main = do
    -- Чтение текста из файла biography.txt
    textExists <- doesFileExist "biography.txt"
    unless textExists $ do
        putStrLn "Файл biography.txt не найден."
        exitFailure

    text <- TIO.readFile "biography.txt"
    -- Удаление пробельных символов для подсчета количества символов без пробелов
    let textNoSpaces = T.filter (not . isSpace) text
    let textLength = T.length textNoSpaces
    when (textLength < 1000) $ do
        putStrLn $ "Текст должен быть не менее 1000 символов без пробелов, а у вас " ++ show textLength
        exitFailure

    -- Запрос сдвига для шифра Цезаря
    putStrLn "Введите сдвиг для шифра Цезаря:"
    shiftStr <- getLine
    let shift = read shiftStr :: Int

    -- Шифрование текста
    let encodedText = caesarEncode shift text

    -- Сохранение зашифрованного текста в файл с указанием сдвига в имени
    let encodedTextFileName = "biography_" ++ show shift ++ ".txt"
    TIO.writeFile encodedTextFileName encodedText
    putStrLn $ "Зашифрованный текст сохранен в файл " ++ encodedTextFileName

    -- Чтение изображения
    imageExists <- doesFileExist "lebedev.bmp"
    unless imageExists $ do
        putStrLn "Файл lebedev.bmp не найден."
        exitFailure

    imageData <- BL.readFile "lebedev.bmp"
    let imageHeader = BL.take 54 imageData -- Заголовок BMP 54 байта
    let imageBody = BL.drop 54 imageData

    -- Подготовка данных текста
    let textData = TE.encodeUtf8 encodedText -- Строгий ByteString
    let textDataLazy = BL.fromStrict textData

    -- Создание изображений с закодированным текстом для n от 1 до 8
    mapM_ (\n -> do
        -- Вычисление вместимости изображения
        let imageCapacityBits = BL.length imageBody * fromIntegral n
        let combinedDataLength = BL.length textDataLazy + 4 -- Добавляем 4 байта для длины
        let textSizeBits = combinedDataLength * 8
        if textSizeBits > imageCapacityBits then
            putStrLn $ "Текст не помещается в изображение при n = " ++ show n
        else do
            -- Кодирование текста в изображение
            let newImageBody = encodeTextIntoImage n imageBody textDataLazy
            let newImageData = BL.append imageHeader newImageBody
            -- Сохранение нового изображения с указанием сдвига и n в имени файла
            let newImageFileName = "lebedev_" ++ show shift ++ "_" ++ show n ++ ".bmp"
            BL.writeFile newImageFileName newImageData
            putStrLn $ "Создан файл " ++ newImageFileName
        ) [1..8]

    -- Запрос имени изображения для декодирования
    putStrLn "Введите имя изображения для декодирования (например, lebedev_10_2.bmp):"
    imageFileName <- getLine

    -- Проверка существования файла
    fileExists <- doesFileExist imageFileName
    unless fileExists $ do
        putStrLn "Файл не найден."
        exitFailure

    -- Извлечение сдвига и n из имени файла
    let (namePart, _) = splitExtension imageFileName
    let nameParts = splitOn "_" namePart
    case nameParts of
        [_, shiftStr', nStr] -> do
            let shift' = read shiftStr' :: Int
            let n' = read nStr :: Int

            -- Чтение изображения
            encodedImageData <- BL.readFile imageFileName
            let encodedImageBody = BL.drop 54 encodedImageData

            -- Декодирование текста из изображения
            let extractedTextDataLazy = decodeTextFromImage n' encodedImageBody
            -- Преобразование в строгий ByteString
            let extractedTextData = BL.toStrict extractedTextDataLazy

            -- Декодирование данных текста как UTF-8
            case TE.decodeUtf8' extractedTextData of
                Left err -> do
                    putStrLn "Ошибка при декодировании текста из изображения."
                    print err
                    exitFailure
                Right extractedTextEncoded -> do
                    -- Расшифровка текста
                    let decodedText = caesarDecode shift' extractedTextEncoded
                    -- Сохранение декодированного текста в файл
                    TIO.writeFile "decoded_text.txt" decodedText
                    putStrLn "Декодированный текст сохранен в файл decoded_text.txt"
        _ -> do
            putStrLn "Имя файла должно быть в формате lebedev_сдвиг_n.bmp"
            exitFailure
