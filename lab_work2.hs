import Data.Char

--1--
toDecimal:: Int -> String -> String
toDecimal _ "" = error "Empty string"
toDecimal 1 snumber = show $ fromTuring snumber (-1) where
            fromTuring "" acc = acc
            fromTuring (s:snum) acc = if s == '1' then fromTuring snum (acc+1) else error "invalid digit detected"
toDecimal base snumber | (base > 62) || (base < 1) = error "Uncorrect base"
                       | otherwise = show $ helper (reverse snumber) 0 0
      where helper "" acc _ = acc
            helper (s:snum) acc count | (ord(s) >= 97) && (ord(s) <= 122) && (ord(s) - 87 < base) = helper snum (acc + (ord(s) - 87) * base ^ count) (count + 1)
                                      | (ord(s) >= 65) && (ord(s) <= 90) && (ord(s) - 29 < base) = helper snum (acc+ (ord(s) - 29) * base ^ count) (count + 1)
                                      | (ord(s) >= 48) && (ord(s) <= 57) && (ord(s) - 48 < base) = helper snum (acc+ (ord(s) - 48) * base ^ count) (count + 1)
                                      | otherwise = error "invalid digit detected"


--2--
fromDecimal:: Int -> String -> String
fromDecimal _ "0" = "0"
fromDecimal _ "" = error "Empty string"
fromDecimal base snumber | (base > 62) || (base < 1) = error "Uncorrect base"
                         | base == 1 = toTuring $ wordToInteger (reverse snumber) 0 0
                         | otherwise = reverse $ helper $ wordToInteger (reverse snumber) 0 0
    where
        --Make number from string
        wordToInteger "" acc _ = acc
        wordToInteger (s:snum) acc dec = acc + wordToInteger snum (digitToInt(s) * 10 ^ dec) (dec + 1)
        --When counting system = 1
        toTuring 0 = "1"
        toTuring acc = "1" ++ toTuring (acc - 1)
        --When counting system > 1
        helper 0 = ""
        helper num | (mod num base) < 10 = (chr ((mod num base) + 48)) : helper (div num base)
                   | (mod num base) >= 10 && (mod num base) <= 35 = (chr ((mod num base) + 87)) : helper (div num base)
                   | (mod num base) >= 36 && (mod num base) <= 61 = (chr ((mod num base) + 29)) : helper (div num base)


--3--
convertFromTo:: Int -> Int -> String -> String
convertFromTo fromBase toBase snumber = fromDecimal toBase (toDecimal fromBase snumber)
