import Data.Char

toDecimal:: Int -> String -> String
toDecimal 1 snumber = show $ fromTuring (reverse snumber) 0 0 1 where
            fromTuring "" s acc dec = s + (acc-1)*dec
            fromTuring (x:snum) s acc dec = if ord(x) == 32 then fromTuring snum (s+(acc-1)*dec) 0 (dec*10) else fromTuring snum s (acc+1) dec
toDecimal base snumber | (base > 61) || (base < 1) = error "Uncorrect base"
                       | otherwise = show (helper (reverse snumber) 0 0)
      where helper "" acc _ = acc
            helper (s:snum) acc count | (ord(s) >= 97) && (ord(s) <= 122) && (ord(s) - 87 < base) = helper snum (acc + (ord(s) - 87) * base ^ count) (count + 1)
                                      | (ord(s) >= 65) && (ord(s) <= 90) && (ord(s) - 29 < base) = helper snum (acc+ (ord(s) - 29) * base ^ count) (count + 1)
                                      | (ord(s) >= 48) && (ord(s) <= 57) && (ord(s) - 48 < base) = helper snum (acc+ (ord(s) - 48) * base ^ count) (count + 1)
                                      | otherwise = error "invalid digit detected"

