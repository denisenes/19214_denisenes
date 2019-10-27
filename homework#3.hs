--Реализация через лямбда-выражения
mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f xs = foldr (\x acc -> f x : acc) [] xs

mapFoldl :: (a -> b) -> [a] -> [b]
mapFoldl f xs = foldl (\acc x -> acc ++ [f x]) [] xs


--Реализация через where-выражения
mapFoldr' :: (a -> b) -> [a] -> [b]
mapFoldr' f xs = foldr g [] xs
    where g x acc = f x : acc

mapFoldl' :: (a -> b) -> [a] -> [b]
mapFoldl' f xs = foldl g [] xs
    where g acc x = acc ++ [f x]
