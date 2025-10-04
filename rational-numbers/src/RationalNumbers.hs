module RationalNumbers
(Rational,
 abs,
 numerator,
 denominator,
 add,
 sub,
 mul,
 div,
 pow,
 expRational,
 expReal,
 rational) where

import Prelude hiding (div, abs, Rational)

-- Data definition -------------------------------------------------------------
data Rational a = Rational a a deriving (Eq, Show)

-- Helper function to compute GCD
gcd' :: Integral a => a -> a -> a
gcd' a 0 = if a < 0 then -a else a
gcd' a b = gcd' b (a `mod` b)

-- Constructor that reduces to lowest terms and ensures standard form
rational :: Integral a => (a, a) -> Rational a
rational (n, d)
  | d == 0    = error "Denominator cannot be zero"
  | otherwise = let g = gcd' n d
                    n' = n `quot` g
                    d' = d `quot` g
                in if d' < 0
                   then Rational (-n') (-d')
                   else Rational n' d'

-- unary operators -------------------------------------------------------------
abs :: Integral a => Rational a -> Rational a
abs (Rational n d) = let absN = if n < 0 then -n else n
                     in rational (absN, d)

numerator :: Integral a => Rational a -> a
numerator (Rational n _) = n

denominator :: Integral a => Rational a -> a
denominator (Rational _ d) = d

-- binary operators ------------------------------------------------------------
add :: Integral a => Rational a -> Rational a -> Rational a
add (Rational a1 b1) (Rational a2 b2) = rational (a1 * b2 + a2 * b1, b1 * b2)

sub :: Integral a => Rational a -> Rational a -> Rational a
sub (Rational a1 b1) (Rational a2 b2) = rational (a1 * b2 - a2 * b1, b1 * b2)

mul :: Integral a => Rational a -> Rational a -> Rational a
mul (Rational a1 b1) (Rational a2 b2) = rational (a1 * a2, b1 * b2)

div :: Integral a => Rational a -> Rational a -> Rational a
div (Rational a1 b1) (Rational a2 b2) = rational (a1 * b2, a2 * b1)

pow :: Integral a => Rational a -> a -> Rational a
pow (Rational n d) e
  | e > 0     = rational (n ^ e, d ^ e)
  | e < 0     = let absE = if e < 0 then -e else e
                in rational (d ^ absE, n ^ absE)
  | otherwise = rational (1, 1)

expRational :: Integral a => Floating b => Rational a -> b -> b
expRational (Rational n d) x = (fromIntegral n ** x) / (fromIntegral d ** x)

expReal :: Floating a => Integral b => a -> Rational b -> a
expReal x (Rational n d) = x ** (fromIntegral n / fromIntegral d)