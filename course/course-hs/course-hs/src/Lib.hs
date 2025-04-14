module Lib
  ( checkStr
  ) where

import Data.Char (digitToInt, isSpace)
import Control.Applicative (Alternative (..), some)
import Data.Functor (($>))

newtype Parser tok a = Parser { runParser :: [tok] -> Maybe ([tok], a) } -- парсер, который принимает список токенов и пытается их разобрать. 
                                                                          -- Если разбор успешен, он возвращает оставшиеся токены и результат разбора; 
                                                                          --если нет — возвращает Nothing.
  
instance Functor (Parser tok) where
  fmap g (Parser p) = Parser $ \xs -> do -- позволяет использовать функцию fmap, чтобы применять функции к результатам разбора
    (rest, result) <- p xs
    Just (rest, g result)

instance Applicative (Parser tok) where
  pure x = Parser $ \xs -> Just (xs, x) -- помещает значение в контекст
  Parser p <*> Parser q = Parser $ \xs -> do --  применения парсера функции к другому парсеру
    (rest, f) <- p xs -- первый парсер p
    (rest', x) <- q rest -- второй парсер q
    Just (rest', f x) -- оставшиеся токены и результат применения функции f к значению x, которое мы получили из второго парсера

instance Monad (Parser tok) where
  return = pure -- помещает значение в монадический контекст
  Parser p >>= f = Parser $ \input -> do 
    (rest, a) <- p input 
    runParser (f a) rest -- Мы вызываем функцию f, передавая ей значение a, чтобы получить новый парсер. 
                          --Затем мы запускаем этот новый парсер на оставшихся токенах rest.

instance Alternative (Parser tok) where
  empty = Parser $ const Nothing -- значение, представляющее "неудачный" парсер
  Parser p <|> Parser q = Parser $ \xs -> -- Если первый парсер p не смог разобрать входные данные, будет вызван второй парсер q
    case p xs of 
      Nothing -> q xs -- у p не получилось
      result  -> result -- у р получилось

satisfy :: (tok -> Bool) -> Parser tok tok
satisfy predicate = Parser $ \input ->
  case input of
    (c : cs) | predicate c -> Just (cs, c) -- разбираем первый токен
    _                      -> Nothing

char :: Eq tok => tok -> Parser tok tok
char c = satisfy (== c) -- парсер, который проверяет, совпадает ли первый токен в входном списке с заданным символом 

spaces :: Parser Char ()
spaces = () <$ many (satisfy isSpace) -- парсер, который принимает один токен (символ) и проверяет, является ли он пробельным
                                      -- many применяет парсер, созданный с помощью satisfy isSpace, и будет продолжать разбирать
                                      -- входные токены, пока они соответствуют условию

lexeme :: Parser Char a -> Parser Char a
lexeme p = p <* spaces  -- p пытается разобрать входные данные.
                        -- Если разбор успешен, парсер spaces будет вызван для обработки любых пробелов, которые могут следовать за результатом p.
                        -- Результат функции lexeme будет равен результату парсера p, а не пробелам.

binaryDigit :: Parser Char Char
binaryDigit = lexeme (satisfy (`elem` "01")) -- парсер, который успешно завершится, если входной символ удовлетворяет этому предикату

binaryNumber :: Parser Char Int
binaryNumber = lexeme $ fmap (foldl (\acc d -> acc * 2 + digitToInt d) 0) (some binaryDigit) -- переводим в двоичное применяя fmap к результату someBinaryDigit

comparisonOp :: Parser Char (Int -> Int -> Bool)
comparisonOp =
      (lexeme (char '=') $> (==)) -- возвращаем функцию (==) для того что написано справа
  <|> (lexeme (char '>') $> (>))
  <|> (lexeme (char '<') $> (<))

parseBinaryComparison :: Parser Char Bool
parseBinaryComparison =
  spaces *> ((\num1 op num2 -> num1 `op` num2) -- убрать пробелы и выполнять дальше независимо от них
                                              -- применяем функцию к результатам дальше
    <$> binaryNumber
    <*> comparisonOp -- последовательно применить парсеры
    <*> binaryNumber) <* spaces

checkStr :: String -> Maybe Bool
checkStr input = case runParser parseBinaryComparison input of -- использует парсер для проверки корректности бинарного 
                                                              -- сравнения в строковом формате и возвращает результат в виде типа Maybe Bool.
  Just ("", result) -> Just result
  _ -> Nothing