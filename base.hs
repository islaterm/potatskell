import           Test.Hspec
import           Test.QuickCheck



{-------------------------------------------}
{--------------  EJERCICIO 1  --------------}
{-------------------------------------------}

{-|
  Efficiently computes the power of a number

  >>> effPow 1 . 5 3
  3.375
  >>> effPow 2 (-5)
  *** Exception : exponente negativo 
-}
effPow :: Num a => a -> Int -> a
effPow _ 0 = 1
effPow b 1 = b
effPow b n | n < 0              = error "exponente negativo"
           | mod n 2 == 0       = bm * bm
           | mod (n - 1) 2 == 0 = b * bm * bm
           | otherwise          = error "caso no implementado"
  where bm = effPow b (div n 2)


{-| 
  Computes the number of bills of $3 and $5 needed to pay for any price greater or equal to $8.

  Parameters:
    n :: Int - The price to pay

  Returns :: (Int, Int) - a pair with the number of $3 and $5 bills (respectively) needed to pay
  
  >>> pay 13
  (1, 2)
  >>> pay 7
  *** Exception: no se pueden pagar 7 pesos
-}
pay :: Int -> (Int, Int)
pay n = if n < 8
  then error ("no se pueden pagar " ++ show n ++ " pesos")
  else payAcc n 0 0

{-| Helper function to calculate the number of $3 and $5 bills.

  Parameters:
    n :: Int    - the price to pay
    acc3 :: Int - the accumulated $3 bills
    acc5 :: Int - the accumulater $5 bills
  
  Returns :: (Int, Int) - a pair with the number of $3 and $5 bills (respectively) needed to pay.
-}
payAcc :: Int -> Int -> Int -> (Int, Int)
payAcc n acc3 acc5 | mod n 5 == 0 = (acc3, acc5 + div n 5)
                   | mod n 3 == 0 = (acc3 + div n 3, acc5)
                   | otherwise    = payAcc (n - 3) (acc3 + 1) acc5




{-------------------------------------------}
{--------------  EJERCICIO 2  --------------}
{-------------------------------------------}

{-| 
  Returns the number of elements of a list that matches a given predicate.

  Parameters:
    predicate :: (a -> Bool) - the predicate function
    (x:xs) :: [a] - the list of elements to compare with the predicate

  Returns :: Int - the number of elements of the list that match the predicate
  
  >>> numberOfHits (> 3) [1..7]
  4
-}
numberOfHits :: (a -> Bool) -> [a] -> Int
numberOfHits _ [] = 0
numberOfHits predicate (x : xs) =
  (if predicate x then 1 else 0) + numberOfHits predicate xs

{-| 
  Splits a list at the first element that matches a given predicate.

  Parameters:
    predicate :: (a -> Bool)  - the predicate function
    (x:xs) :: [a]             - the list of elements to compare with the predicate

  Returns :: ([a], [a]) - the splitted list
  
  >>> splitAtFirstHit (> 3) [1..7]
  ([1, 2, 3], [4, 5, 6, 7])
  >>> splitAtFirstHit even [2, 3, 4]
  ([], [2, 3, 4])
  >>> splitAtFirstHit even [1, 3, 5]
  *** Exception: no hit in the list
-}
splitAtFirstHit :: (a -> Bool) -> [a] -> ([a], [a])
splitAtFirstHit _         []       = error "no hit in the list"
splitAtFirstHit predicate (x : xs) = withAcc predicate [] (x : xs)
 where
  withAcc _ _ [] = error "no hit in the list"
  withAcc predicate' acc (y : ys) =
    if predicate' y then (acc, y : ys) else withAcc predicate' (acc ++ [y]) ys

{-| 
  Gets the index of every element of a list that matches a given predicate.

  Parameters:
    predicate :: (a -> Bool)  - the predicate function
    (x:xs) :: [a]             - the list of elements to compare with the predicate

  Returns :: [a] -  the indices of the matched elements
  
  >>> positionAllHits even [4..8]
  [0, 2, 4]
  >>> positionAllHits even [1, 3, 5]
  []
-}
positionAllHits :: (a -> Bool) -> [a] -> [Int]
positionAllHits _         []       = []
positionAllHits predicate (x : xs) = withAcc predicate (x : xs) 0 []
 where
  withAcc _ [] _ _ = []
  withAcc predicate' (y : ys) idx acc =
    ([ idx | predicate' y ]) ++ withAcc predicate' ys (idx + 1) acc

{-| 
  Returns the elements of a list that are in an even position.

  >>> evens "funcional"
  "fninl"
-}
evens :: [a] -> [a]
evens []       = []
evens (x : xs) = x : odds xs

{-| 
  Returns the elements of a list that are in an odd position.

  >>> odds "funcional"
  "ucoa"
-}
odds :: [a] -> [a]
odds []       = []
odds (_ : xs) = evens xs




{-------------------------------------------}
{--------------  EJERCICIO 3  --------------}
{-------------------------------------------}

{-| 
  Determines if an element of a list matches a certain predicate

  Parameters:
    predicate :: (a -> Bool)  - the predicate function
    (x:xs) :: [a]             - the list of elements to compare with the predicate

  Returns :: Bool - True if a value matches the predicate, False otherwise
  
  >>> hasSomeHit even [2, 3]
  True
  >>> hasSomeHit even [1, 3]
  False
-}
hasSomeHit :: (a -> Bool) -> [a] -> Bool
hasSomeHit _         []       = False
hasSomeHit predicate (x : xs) = predicate x || hasSomeHit predicate xs

{-| 
  Determines if an element is on a list

  Parameters:
    find  :: a      - the searched element
    (x:xs)  :: [a]  - the list of elements

  Returns :: Bool - True if the searched element is on the list, False otherwise
  
  >>> isMember 'a' "funcional"
  True
  >>> isMember 5 [1, 2, 3]
  False
-}
isMember :: Eq a => a -> [a] -> Bool
isMember _    []       = False
isMember find (x : xs) = hasSomeHit (find ==) (x : xs)

{-| 
  Determines if there're repeated elements on a list

  Parameters:
    (x:xs)  :: [a]  - the list of elements

  Returns :: Bool - True if the list has repeated elements, False otherwise
  
  >>> repeatedElem [1, 2, 3, 2, 5]
  True
  >>> repeatedElem [1, 2, 3]
  False
-}
repeatedElem :: Eq a => [a] -> Bool
repeatedElem []       = False
repeatedElem (x : xs) = isMember x xs || repeatedElem xs

{-|
  Repeatedly applies a function until the result matches a predicate.
  
  Parameters:
    fun :: (a -> a)           - the function to be applied
    predicate :: (a -> Bool)  - the predicate function
    base :: a                 - the starting value

  Returns :: a  - The result of applying the function multiple times over the base

  >>> applyUntil (+ 2) (> 6) 2
  8
  >>> applyUntil (+ 2) (> 1) 2
  2
-}
applyUntil :: (a -> a) -> (a -> Bool) -> a -> a
applyUntil fun predicate base =
  if predicate base then base else applyUntil fun predicate (fun base)

{-|
  Returns the smaller power of two that's greater or equal to n.  

  >>> leastPow2 11
  16
  >>> leastPow2 (-11)
  *** Exception: argumento negativo
-}
leastPow2 :: Int -> Int
leastPow2 n =
  if n < 0 then error "argumento negativo" else applyUntil (* 2) (>= n) 1

{-|
  Returns the largest suffix of a boolean list that contains the same amount of True and False 
  values.

  >>> balancedSuffix [True, True, True, False, False, True]
  [True, False, False, True]
-}
balancedSuffix :: [Bool] -> [Bool]

balancedSuffix [] = []
balancedSuffix _  = error "caso no implementado"




{-------------------------------------------}
{--------------  EJERCICIO 4  --------------}
{-------------------------------------------}

{-| 
  Determines if an element is on a list

  Parameters:
    find  :: a      - the searched element
    (x:xs)  :: [a]  - the list of elements

  Returns :: Bool - True if the searched element is on the list, False otherwise
  
  >>> isMemberPF 'a' "funcional"
  True
  >>> isMemberPF 5 [1, 2, 3]
  False
-}
isMemberPF :: Eq a => a -> [a] -> Bool
isMemberPF = hasSomeHit . (==)



{-------------------------------------------}
{--------------  EJERCICIO 5  --------------}
{-------------------------------------------}

testEffPow :: Spec
testEffPow = describe "effPow function:" $ do
  it "For all a, b, n and n >= 0 => (a * b)^n = a^n * b^n"
    $ property
    $ \a b n ->
        n
          >=  0
          ==> effPow ((a :: Int) * b) (n :: Int)
          ==  (effPow a n * effPow b n)
  it "For all a, m, n and m, n >= 0 => a^(m + n) == a^m * a^n"
    $ property
    $ \a m n ->
        ((m :: Int) >= 0)
          &&  ((n :: Int) >= 0)
          ==> effPow (a :: Int) (m + n)
          ==  effPow a          m
          *   effPow a          n

testPay :: Spec
testPay =
  describe "pay function:"
    $ it "For all n >= 8 => 3a + 5n = n"
    $ property
    $ \n -> (n :: Int) >= 8 ==> (3 * fst (pay n) + 5 * snd (pay n)) == n

testNumberOfHits :: Spec
testNumberOfHits = describe "numberOfHits function:" $ do
  it
      "For all xs, ys : numberOfHits even (xs ++ ys) = numberOfHits xs + numberOfHits ys"
    $ property
    $ \xs ys ->
        numberOfHits even ((xs :: [Int]) ++ (ys :: [Int]))
          == numberOfHits even xs
          +  numberOfHits even ys
  it "For all xs : numberOfHits True xs = |xs|" $ property $ \xs ->
    numberOfHits (const True) (xs :: [Int]) == length xs
  it "For all xs : numberOfHits False xs = 0" $ property $ \xs ->
    numberOfHits (const False) (xs :: [Int]) == 0

testExtraNumberOfHits :: Spec
testExtraNumberOfHits =
  describe "Inclusion-Exclusion principle:"
    $ it
        "For all predicate p, q : |p(xs) or q(xs)| = |p(xs)| + |q(xs)| - |p(xs) and q(xs)|"
    $ property
    $ \xs a b -> (a::Int) > 0 && (b::Int) > 0 ==>
        numberOfHits (\x -> mod x a == 0 || mod x b == 0) (xs :: [Int])
          == numberOfHits (\x -> mod x a == 0)                 xs
          +  numberOfHits (\x -> mod x b == 0)                 xs
          -  numberOfHits (\x -> mod x a == 0 && mod x b == 0) xs


main :: IO ()
main = hspec $ do
  testEffPow
  testPay
  testNumberOfHits
  testExtraNumberOfHits

