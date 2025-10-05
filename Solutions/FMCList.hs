{-# LANGUAGE GADTs #-}

module FMCList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C

{- import qualified ... as ... ?

To use a function from a qualified import
you need to prefix its name with its alias and a dot:
P.head   C.toUpper   etc.

I import these for you to test the original functions on ghci:

ghci> :t C.toUpper
C.toUpper :: Char -> Char

You MUST NOT use ANY of these in your code

-}


{- Our lists vs Haskell lists

Our definition:

data List a where
  Nil  :: List a
  Cons :: a -> List a -> List a

Here we use Haskell's built-in lists and associated syntactic sugar.
It is as if it was defined like this:

    data [a] = [] | (x : xs)

or like this:

    data [a] where
      []  :: [a]
      (:) :: a -> [a] -> [a]

write [a]       for our List a
write []        for our List
write []        for our Nil
write (x : xs)  for our Cons x xs
write [u,v]     for our u `Cons` (v `Cons` Nil)

-}

head :: [a] -> a
head [] = error "Nil"
head (x:_) = x

tail :: [a] -> [a]
tail [] = error "Nil"
tail (_:x) = x

null :: [a] -> Bool
null [] = True
null _ = False

length :: Integral i => [a] -> i
length [] = 0
length (_:xs) = 1 + length xs

sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs

product :: Num a => [a] -> a
product [] = 1
product (x:xs) = x * product xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc x [] = [x]
snoc x (y:ys) = y:snoc x ys

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?!)
infixl 5 +++

minimum :: Ord a => [a] -> a
minimum [] = error "Nil"
minimum [x] = x
minimum (x:xs) = min x (minimum xs)

maximum :: Ord a => [a] -> a
maximum [] = error "Nil"
maximum [x] = x
maximum (x:xs) = max x (maximum xs)

take :: Integral i => i -> [a] -> [a]
take i _ | i <= 0 = []
take _ [] = []
take i (x:xs) = x : take (i-1) xs

drop :: Integral i => i -> [a] -> [a]
drop i xs | i <= 0 = xs
drop _ [] = []
drop i (_:xs) = drop (i-1) xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile b (x:xs)
  | b x = x : takeWhile b xs
  | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile b (x:xs)
  | b x = dropWhile b xs
  | otherwise = x:xs

tails :: [a] -> [[a]]
tails [] = [[]]
tails (x:xs) = (x:xs):tails xs

init :: [a] -> [a]
init [] = error "Nil"
init [x] = []
init (x:xs) = x:init xs

inits :: [a] -> [[a]]
inits [] = [[]]
inits (x:xs) = []: [x:ys | ys <- inits xs]

subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:xs) = subsequences xs ++ [x:ys | ys <- subsequences xs]

any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any b (x:xs) = b x || any b xs

all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all b (x:xs) = b x && all b xs

and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && and xs

or :: [Bool] -> Bool
or [] = False
or (x:xs) = x || or xs

concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ concat xs

-- elem using the funciton 'any' above
elem :: Eq a => a -> [a] -> Bool
elem x = any (== x)

-- elem': same as elem but elementary definition
-- (without using other functions except (==))
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys) = x == y || elem' x ys

(!!) :: [a] -> Int -> a
[] !! _ = error "nil"
(x:_) !! 0 = x
(_:xs) !! i
  | i < 0 = error "negative"
  | otherwise = xs !! (i-1)

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter b (x:xs)
  | b x = x : filter b xs
  | otherwise = filter b xs

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

cycle :: [a] -> [a]
cycle [] = error "nil"
cycle xs = xs ++ cycle xs

repeat :: a -> [a]
repeat x = x : repeat x

replicate :: Int -> a -> [a]
replicate i x
  | i <= 0 = []
  | otherwise = x : replicate (i-1) x

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys 

isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf [] _ = True
isInfixOf _ [] = False
isInfixOf xs ys = any (isPrefixOf xs) (tails ys)

isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf xs ys = reverse xs `isPrefixOf` reverse ys

zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

intercalate :: [a] -> [[a]] -> [a]
intercalate _ [] = []
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)

splitAt :: Int -> [a] -> ([a], [a])
splitAt i xs = (takeWhile i xs, drop i xs)
  where
    takeWhile 0 _ = []
    takeWhile _ [] = []
    takeWhile m (y:ys) = y : takeWhile (m-1) ys

-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

break :: (a -> Bool) -> [a] -> ([a], [a])
break _ [] = ([], [])
break b (x:xs)
  | b x = ([], x:xs)
  | otherwise = (x:ys, zs)
  where
    (ys, zs) = break b xs

-- lines
-- words
-- unlines
-- unwords

-- transpose

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome = undefined

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

