import Prelude
import Data.List
import Data.Char

type Key = Integer
type Value = String
data HashTable k v = HashTable { arr:: [[(k, v)]],
                                 cnt:: Integer,
                                 cap:: Integer } deriving (Show)

-----------Hash-function-----------------------------------
hashFoo:: Integer -> String -> Int
hashFoo size str = (foldl (\y x -> (ord x) + y) 0 str) `mod` fromIntegral size
-----------------------------------------------------------

defaultSize::Integer
defaultSize = 10

defaultHashTable:: HashTable k v
defaultHashTable = HashTable { arr = (replicate (fromIntegral defaultSize) []),
                               cnt = 0,
                               cap = defaultSize }

fromList::(Show k, Eq k)=> [(k,v)] -> HashTable k v
fromList list = foldl (\y x -> (insertHash y (fst x) (snd x))) table list where
   table = HashTable { arr = (replicate (len * 4) []),
             cnt = 0,
             cap = fromIntegral $ len * 4 }
   len = length list

rehash::(Show k, Eq k) => HashTable k v -> HashTable k v
rehash (HashTable tab ccnt ccap) = fromList (toList tab) where
      toList xs = foldl (++) [] xs

insertHash::(Show k, Eq k) => HashTable k v -> k -> v -> HashTable k v
insertHash (HashTable list ccnt ccap) key val | ccnt * 2 < ccap = HashTable { arr = ((fst twoParts) ++ (insertInPart $ snd twoParts)),
                                                             cnt = ccnt + 1,
                                                             cap = ccap }
                                              | otherwise = insertHash (rehash (HashTable list ccnt ccap)) key val
   where
   twoParts = splitAt hash list
   insertInPart part = (((key, val):(head part)) : (tail part))
   hash = hashFoo ccap (show key)

clearHash::HashTable k v -> HashTable k v
clearHash (HashTable _ _ ccap) = HashTable { arr = (replicate (fromIntegral ccap) []),
                                             cnt = 0,
                                             cap = ccap } --TODO

eraseByKey:: (Show k, Eq k) => HashTable k v -> k -> HashTable k v
eraseByKey (HashTable list ccnt ccap) key = HashTable { arr = ((fst twoParts) ++ (eraseFromPart $ snd twoParts)),
                                                        cnt = ccnt + 1,
                                                        cap = ccap } where
   twoParts = splitAt hash list
   eraseFromPart part = ((deleteFromList $ head part):(tail part))
   deleteFromList llist = filter (\(k, _) -> k /= key) llist
   hash = hashFoo ccap $ show key

at::(Show k, Eq k)=>HashTable k v -> k -> Maybe v
at (HashTable tab _ ccap ) key = if (null list) then Nothing else Just (head list) where
   twoParts = splitAt hash tab
   list = map (\( _, y) -> y) $ filter (\(x, _) -> x == key) (head $ snd twoParts)
   hash = hashFoo ccap $ show key

contains::(Show k, Eq k)=>HashTable k v -> k -> Bool;
contains (HashTable tab _ ccap ) key = elem key list where
   twoParts = splitAt hash tab
   list = map (\(x, _) -> x) (head $ snd twoParts)
   hash = hashFoo ccap $ show key

sizeHash::(Show k, Eq k) => HashTable k v -> Integer;
sizeHash (HashTable _ _ ccap) = ccap 

emptyHash::(Show k, Eq k) => HashTable k v -> Bool;
emptyHash (HashTable _ ccnt _) = if ccnt == 0 then True else False


--tests--
l = [(1,"preved"),(2,"ko"),(3,"kreved"),(4,"ko"),(2,"foo")]
tab = fromList l
