{-# LANGUAGE GADTs #-}

module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral(..)
    , Bool(..) , not , (&&) , (||)
    , ($)
    , (.)
    , (++)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat where
  O :: Nat
  S :: Nat -> Nat

----------------------------------------------------------------
-- typeclass implementations
----------------------------------------------------------------

instance Show Nat where

    -- zero  should be shown as O
    -- three should be shown as SSSO
    show O = "O"
    show (S n) = "S" ++ show n

instance Eq Nat where

    O == O = True
    O == (S _) = False
    (S _) == O = False
    (S n) == (S m) = n == m

instance Ord Nat where

    O <= O = True
    O <= (S _) = True
    (S _) <= O = False
    (S n) <= (S m) = n <= m

    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min O _ = O
    min _ O = O
    min (S n) (S m) = S (min n m)

    max O n = n
    max n O = n
    max (S n) (S m) = S (max n m)


----------------------------------------------------------------
-- internalized predicates
----------------------------------------------------------------

isZero :: Nat -> Bool
isZero O = True
isZero (S _) = False

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O = O
pred (S n) = n

even :: Nat -> Bool
even O = True
even (S O) = False
even (S (S n)) = even n

odd :: Nat -> Bool
odd O = False
odd (S O) = True
odd (S (S n)) = odd n


----------------------------------------------------------------
-- operations
----------------------------------------------------------------

-- addition
(<+>) :: Nat -> Nat -> Nat
n <+> O = n
n <+> S m = S (n <+> m)


-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
monus n O = n
monus O m = O
monus (S n) (S m) = monus n m

(<->) :: Nat -> Nat -> Nat
(<->) = monus

-- multiplication
times :: Nat -> Nat -> Nat
times O _ = O
times _ O = O
times n (S m) = n <+> times n m

(<*>) :: Nat -> Nat -> Nat
(<*>) = times

-- power / exponentiation
pow :: Nat -> Nat -> Nat
pow n O = S O
pow n (S m) = times n (pow n m)

exp :: Nat -> Nat -> Nat
exp = pow

(<^>) :: Nat -> Nat -> Nat
(<^>) = pow

infixr 8 <^>

-- quotient
(</>) :: Nat -> Nat -> Nat
_ </> O = error "Division by O is undefined"
n </> m =
  case n < m of
    True -> O
    False -> S ((n `monus` m) </> m)

-- remainder
(<%>) :: Nat -> Nat -> Nat
_ <%> O = error "Division by O is undefined"
n <%> m = 
  case n < m of
    True -> n
    False -> (n `monus` m) <%> m

-- euclidean division
eucdiv :: (Nat, Nat) -> (Nat, Nat)
eucdiv (n, m) = (n </> m, n <%> m)

-- divides
(<|>) :: Nat -> Nat -> Bool
n <|> m = m <%> n == O

divides = (<|>)


-- distance between nats
-- x `dist` y = |x - y|
-- (Careful here: this - is the real minus operator!)
dist :: Nat -> Nat -> Nat
dist n m
    | n >= m = n `monus` m
    | otherwise = m `monus` n

(|-|) = dist

factorial :: Nat -> Nat
factorial O = S O
factorial (S n) = S n * factorial n

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = O
sg (S _) = S O

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo O _ = error "Log of base zero is undefined"
lo (S O) _ = error "Log of base one is undefined"
lo _ O = error "Log of zero is undefined"
lo b a =
  case a < b of
    True -> O
    False -> S (lo b (a</>b))


----------------------------------------------------------------
-- Num & Integral fun
----------------------------------------------------------------

-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!

toNat :: Integral a => a -> Nat
toNat 0 = O
toNat n
    | n < 0 = error "Negative numbers can't be converted to Nat"
    | otherwise = S (toNat (n - 1))

fromNat :: Integral a => Nat -> a
fromNat O = 0
fromNat (S n) = 1 + fromNat n


-- Voilá: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
      | x < 0     = error "Negative numbers can't be converted to Nat"
      | x == 0    = O
      | otherwise = S (fromInteger (x - 1))

