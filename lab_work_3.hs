-----------Data-header-------------------------------------------------------------------
data Complex a = Complex a a
data QuantumState a = QuantumState a String
type Qubit a = [QuantumState a]


-----------Classes-header----------------------------------------------------------------
instance (Eq a) => Eq (Complex a) where
    (Complex real1 imag1) == (Complex real2 imag2) = (real1 == real2) && (imag1 == imag2)

instance (Show a) => Show (Complex a) where
    show (Complex real image) = show real ++ "+" ++ show image ++ "i"

instance (Show a) => Show (QuantumState a) where
    show (QuantumState complex label) = show complex ++ " " ++ label

instance (Eq a) => Eq (QuantumState a) where
    (QuantumState complex1 label1) == (QuantumState complex2 label2) = (complex1 == complex2) && (label1 == label2)

instance (Num a, Eq a, Ord a) => Num (Complex a) where
    --(+):: (Num a) => Complex a -> Complex a -> Complex a
    (+) (Complex real1 image1) (Complex real2 image2) = Complex (real1 + real2) (image1 + image2)
    --(*):: (Num a) => Complex a -> Complex a -> Complex a
    (*) (Complex real1 image1) (Complex real2 image2) = (Complex (real1*real1 - image1*image2) (image2*real2 + real1*image1))
    abs (Complex real image) = (Complex (real*real + image*image) 0)
    signum (Complex real image) | real < 0 = (Complex (-1) 0)
                                | real == 0 = (Complex 0 0)
                                | real > 0 = (Complex 1 0)
    negate (Complex real image) = (Complex (-real) image)

instance Functor QuantumState where
    fmap f (QuantumState complex label) = QuantumState (f complex) label


----------Functions----------------------------------------------------------------------

toList:: Qubit a -> [a]
toList [] = []
toList ((QuantumState complex _ ):xs) = (complex : (toList xs))

toLabelList:: Qubit a->[String]
toLabelList [] = []
toLabelList ((QuantumState _ str):xs) = (str:(toLabelList xs))

fromList:: [a]->[String]->Qubit a
fromList [] [] = []
fromList (x:xs) (y:ys) = (QuantumState x y):fromList xs ys

toPairList:: Qubit a->[(a,String)]
toPairList [] = []
toPairList ((QuantumState complex str):xs) = (complex, str):toPairList xs

fromPairList:: [(Complex a, String)] -> Qubit (Complex a)
fromPairList [] = []
fromPairList ((complex, str):xs) = (QuantumState complex str):fromPairList xs

scalarProduct:: (Num a, Ord a) => Qubit (Complex a) -> Qubit (Complex a) -> (Complex a)
scalarProduct xs ys = foldl1 (+) (zipWith (\(QuantumState comp1 _) (QuantumState comp2 _) -> comp1 * comp2) xs ys)

entagle:: (Num a, Ord a, Eq a) => Qubit (Complex a) -> Qubit (Complex a) -> Qubit (Complex a)
entagle [] [] = []
entagle xs ys = [(QuantumState (c1*c2) (l1++l2))  | (QuantumState c1 l1) <- xs, (QuantumState c2 l2) <- ys]


x = (Complex 4 7)
y = (Complex 1 3)

q = QuantumState x "uuu"
w = QuantumState y "www"
q2 = QuantumState x "aaa"
q3 = QuantumState (Complex 12 32) "cucu"

qubit = [q, q2, q3]
qub = [q, w]
