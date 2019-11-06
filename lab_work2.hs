import Data.Char
import Data.List
import Prelude

--1--
toDecimal:: Int -> String -> String
toDecimal _ "" = error "Empty string"
toDecimal 1 snumber = show $ fromTuring snumber (-1) where
            fromTuring "" acc = acc
            fromTuring (s:snum) acc = if s == '1' then fromTuring snum (acc+1) else error "invalid digit detected"
toDecimal base snumber | (base > 62) || (base < 1) = error "Uncorrect base"
                       | otherwise = show $ helper  snumber 0 0
      where alphabet = ['0'..'9']++['a'..'z']++['A'..'Z']
            fromMaybe (Just a) = a
            helper (s:snum) acc count | (elem s alphabet) = foldl (\acc x -> acc * base + (fromMaybe(elemIndex x alphabet))) (fromMaybe(elemIndex s alphabet)) snum
                                      | otherwise = error "invalid digit detected"


--2--
fromDecimal:: Int -> String -> String
fromDecimal 1 snum = replicate ((read snum) + 1) '1'
fromDecimal _ "0" = "0"
fromDecimal _ "" = error "Empty string"
fromDecimal base snumber | (base > 62) || (base < 1) = error "Uncorrect base"
                         | otherwise = reverse $ helper $ (read snumber)
    where
        alphabet = ['0'..'9']++['a'..'z']++['A'..'Z']
        helper 0 = ""
        helper num = (alphabet !! (mod num base)) : helper (div num base)


--3--
convertFromTo:: Int -> Int -> String -> String
convertFromTo fromBase toBase snumber = fromDecimal toBase (toDecimal fromBase snumber)

--comments have to be rewrited
