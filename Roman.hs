-- Roman.hs
-- A simple module for Roman numbers
-- (c) 2019 Thomas Angarano

module Roman
    (
      RomanException,
      parseIfRoman,
      validateRoman,
      toRoman
    ) where

import Control.Exception

data RomanException = NotRomanLiteralException | OutOfBoundsException deriving Show

instance Exception RomanException


parseIfRoman :: Num a => [Char] -> Maybe a
parseIfRoman x = if validateRoman x then Just (parseRoman x) else Nothing


isRomanLiteral :: Char -> Bool
isRomanLiteral x = x `elem` "IVXLCDM"


validateRoman :: [Char] -> Bool
validateRoman "N" = True
validateRoman [] = False
validateRoman (x:xs)
    | xs == [] = isRomanLiteral x
    | otherwise = (t > n || (x `elem` "IXCM" && (t * 10) >= n))
                  && (x /= h || l < 3 || (x /= xs !! 1 || x /= xs !! 2))
                  && validateRoman xs
        where l = length xs
              t = if isRomanLiteral x then parseRoman [x] else 0
              h = head xs
              n = if isRomanLiteral h then parseRoman [h] else 0


-- used internally by `parseIfRoman`
-- `parseRoman` is not as rigid as `validateRoman` in what Strings it accepts
-- for example
--  ` parseRoman "CMXCIX"` returns `999`
--  ` parseRoman "IM"` also returns `999`
--  ` validateRoman "CMXCIX"` returns `True`
--  ` validateRoman "IM"` returns `False`
--  throws `NotRomanLiteralException` if the String contains characters other
--  than "IVXLCDMN" or is not "N"
parseRoman :: Num a => [Char] -> a
parseRoman "" = throw NotRomanLiteralException
parseRoman (x:[])
    | x == 'I' = 1
    | x == 'V' = 5
    | x == 'X' = 10
    | x == 'L' = 50
    | x == 'C' = 100
    | x == 'D' = 500
    | x == 'M' = 1000
    | x == 'N' = 0
parseRoman xs = parse' xs 0
    where
        parse' :: Num a => [Char] -> a -> a
        parse' [] y = y
        parse' (x:xs) y
            | isRomanLiteral x == False = throw NotRomanLiteralException
            | xs == [] = y + parseRoman [x]
            | parseRoman [head xs] > parseRoman [x] = parse' (tail xs) (y + parseRoman [head xs] - parseRoman [x])
            | otherwise = parse' xs (y + parseRoman [x])


toRoman :: Integral a => a -> [Char]
toRoman 0 = "N"
toRoman x
    | x < 0 = ""
    | x > 3999 = throw OutOfBoundsException
    | x == 1 = "I"
    | x == 5 = "V"
    | x == 10 = "X"
    | x == 50 = "L"
    | x == 100 = "C"
    | x == 500 = "D"
    | x == 1000 = "M"
    | otherwise = let
        tor :: Integral a => a -> Int -> [Char] -> [Char]
        tor 0 _ l = l
        tor x r l
            | d == 0 = tor (x `div` 10) (r * 10) l
            | d == 4 = tor (x `div` 10) (r * 10) (toRoman r ++ toRoman (r * 5) ++ l)
            | d == 9 = tor (x `div` 10) (r * 10) (toRoman r ++ toRoman (r * 10) ++ l)
            | d == 5 = tor (x `div` 10) (r * 10) $ toRoman (r * 5) ++ l
            | d == 1 = tor (x `div` 10) (r * 10) $ (toRoman r) ++ l
            | otherwise = tor (x - 1) r $ head (toRoman r) : l
            where d = x `mod` 10
        in tor x 1 ""
