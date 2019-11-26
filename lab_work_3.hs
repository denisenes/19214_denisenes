-----------Data-header-------------------------------------------------------------------
data Complex a = Complex a a
data QuantumState a = QuantumState a String
type Qubit a = [QuantumState a]


-----------Interface-header----------------------------------------------------------------
instance (Eq a) => Eq (Complex a) where
    (Complex real1 imag1) == (Complex real2 imag2) = (real1 == real2) && (imag1 == imag2)

instance (Show a) => Show (Complex a) where
    show (Complex real image) = show real ++ "+" ++ show image ++ "i"

instance (Show a) => Show (QuantumState a) where
    show (QuantumState complex label) = show complex ++ " " ++ label

instance (Eq a) => Eq (QuantumState a) where
    (QuantumState complex1 label1) == (QuantumState complex2 label2) = (complex1 == complex2) && (label1 == label2)

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

scalarProduct:: (Num a) => Qubit (Complex a) ->Qubit (Complex a) -> (Complex a)
scalarProduct [] [] = Complex 0 0
scalarProduct ((QuantumState comp1 _):xs) ((QuantumState comp2 _):ys) = mplus (mproduct comp1 comp2) (scalarProduct xs ys) where
    --mplus:: (Num a) => Complex a -> Complex a -> Complex a
    mplus (Complex real1 image1) (Complex real2 image2) = Complex (real1 + real2) (image1 + image2)
    --mproduct:: (Num a) => Complex a -> Complex a -> Complex a
    mproduct (Complex real1 image1) (Complex real2 image2) = (Complex (real1*real1 - image1*image2) (image2*real2 + real1*image1))

entagle:: (Num a) => Qubit (Complex a) -> Qubit (Complex a) -> Qubit (Complex a)
entagle [] [] = []
entagle ((QuantumState complex1 label1):xs) ((QuantumState complex2 label2):ys) = ((QuantumState (mproduct complex1 complex2) (label1++label2)):entagle xs ys) where
--mproduct:: (Num a) => Complex a -> Complex a -> Complex a
    mproduct (Complex real1 image1) (Complex real2 image2) = (Complex (real1*real1 - image1*image2) (image2*real2 + real1*image1))


x = (Complex 4 7)
y = (Complex 1 3)

q = QuantumState x "uuu"
w = QuantumState y "www"
q2 = QuantumState x "aaa"
q3 = QuantumState (Complex 12 32) "cucold"

qubit = [q, q2, q3]
qub = [q, w]
