-- Файл: Lib.hs
module Lib (
    caesarEncode,
    caesarDecode,
    encodeTextIntoImage,
    decodeTextFromImage
) where

import Data.Bits
import Data.Word
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Binary.Put (runPut, putWord32be)
import Data.Binary.Get (runGet, getWord32be)

-- Списки англ букв в верхнем и нижнем регистре
uppercaseLetters :: [Char]
uppercaseLetters = ['A'..'Z']
lowercaseLetters :: [Char]
lowercaseLetters = ['a'..'z']

-- Функция сдвига символа в алфавите
shiftChar :: Int -> Char -> Char
shiftChar offset c
    | c `elem` uppercaseLetters = shiftInList offset c uppercaseLetters
    | c `elem` lowercaseLetters = shiftInList offset c lowercaseLetters
    | otherwise = c -- Оставляем прочие символы без изменений

-- Функция сдвига символа в заданном списке символов
shiftInList :: Int -> Char -> [Char] -> Char
shiftInList offset c lst =
    let idx = T.findIndex (== c) (T.pack lst)
        len = length lst
    in case idx of
        Just i -> lst !! ((i + offset) `mod` len)
        Nothing -> c -- Не должно произойти

-- Функции шифрования и дешифрования шифром Цезаря
caesarEncode :: Int -> T.Text -> T.Text
caesarEncode offset = T.map (shiftChar offset)

caesarDecode :: Int -> T.Text -> T.Text
caesarDecode offset = T.map (shiftChar (-offset))

-- Преобразование байтов в биты
bytesToBits :: BL.ByteString -> [Bool]
bytesToBits = concatMap byteToBits . BL.unpack

-- Преобразование байта в список битов (от старшего к младшему)
byteToBits :: Word8 -> [Bool]
byteToBits byte = [testBit byte i | i <- [7,6..0]]

-- Преобразование битов в байты
bitsToBytes :: [Bool] -> BL.ByteString
bitsToBytes bits = BL.pack $ bitsToBytes' bits

bitsToBytes' :: [Bool] -> [Word8]
bitsToBytes' [] = []
bitsToBytes' bits =
    let (byteBits, rest) = splitAt 8 bits
        byte = bitsToWord8 byteBits
    in byte : bitsToBytes' rest

-- Преобразование списка битов в байт
bitsToWord8 :: [Bool] -> Word8
bitsToWord8 bits = foldl setBitIfTrue 0 (zip [7,6..0] bitsPadded)
    where
        bitsPadded = take 8 (bits ++ repeat False)
        setBitIfTrue acc (i, True) = setBit acc i
        setBitIfTrue acc (_, False) = acc

-- Функция замены n младших битов в каждом байте изображения на биты текста
encodeTextIntoImage :: Int -> BL.ByteString -> BL.ByteString -> BL.ByteString
encodeTextIntoImage n imageData textData =
    let textLength = BL.length textData
        -- Преобразование длины текста в 4 байта (32 бита) в формате big-endian
        textLengthData = runPut (putWord32be (fromIntegral textLength))
        -- Объединение данных длины и текста
        combinedData = BL.append textLengthData textData
        imageBytes = BL.unpack imageData
        textBits = bytesToBits combinedData
        -- Замена младших n битов в каждом байте изображения
        newImageBytes = replaceBitsInBytes n imageBytes textBits
    in BL.pack newImageBytes

-- Замена младших n битов в каждом байте
replaceBitsInBytes :: Int -> [Word8] -> [Bool] -> [Word8]
replaceBitsInBytes _ [] _ = []
replaceBitsInBytes _ imageBytes [] = imageBytes
replaceBitsInBytes n (byte:bytes) bits =
    let (textBitsForByte, restBits) = splitAt n bits
        newByte = replaceLastNBits n byte textBitsForByte
    in newByte : replaceBitsInBytes n bytes restBits

-- Замена младших n битов в байте
replaceLastNBits :: Int -> Word8 -> [Bool] -> Word8
replaceLastNBits n byte bits =
    let mask = complement (bitMask n)
        byteCleared = byte .&. mask
        bitsValue = bitsToValue bits
    in byteCleared .|. bitsValue

-- Создание маски для очистки младших n битов
bitMask :: Int -> Word8
bitMask n = foldl setBit 0 [i | i <- [0..(n-1)]]

-- Преобразование списка битов в значение Word8
bitsToValue :: [Bool] -> Word8
bitsToValue bits = foldl setBitIfTrue 0 (zip [n-1, n-2..0] bitsPadded)
    where
        n = length bits
        bitsPadded = take n (bits ++ repeat False)
        setBitIfTrue acc (i, True) = setBit acc i
        setBitIfTrue acc (_, False) = acc

-- Функция извлечения текста из изображения
decodeTextFromImage :: Int -> BL.ByteString -> BL.ByteString
decodeTextFromImage n imageData =
    let imageBytes = BL.unpack imageData
        bits = extractBitsFromBytes n imageBytes
        -- Извлекаем первые 32 бита для получения длины текста
        (lengthBits, restBits) = splitAt 32 bits
        lengthBytes = bitsToBytes lengthBits
        textLength = runGet getWord32be lengthBytes
        numTextBits = fromIntegral textLength * 8
        -- Извлечение битов текста на основе длины
        (textBits, _) = splitAt numTextBits restBits
        textData = bitsToBytes textBits
    in textData

-- Извлечение младших n битов из каждого байта
extractBitsFromBytes :: Int -> [Word8] -> [Bool]
extractBitsFromBytes _ [] = []
extractBitsFromBytes n (byte:bytes) =
    let bits = byteToBits byte
        extractedBits = drop (8 - n) bits
    in extractedBits ++ extractBitsFromBytes n bytes

