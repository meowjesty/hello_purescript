module Data.Path where

import Prelude

import Data.Array (tail)
import Data.Foldable (foldl, sum)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Global as Global
import Math as Math

gcd_ :: Int -> Int -> Int
gcd_ n 0 = n
gcd_ 0 m = m
gcd_ n m =
    if n > m then
        gcd_ (n - m) m
    else
        gcd_ n (m - n)

gcd__ :: Int -> Int -> Int
gcd__ n 0 = n
gcd__ 0 m = m
gcd__ n m
    | n > m = gcd__ (n - m) m
    | otherwise = gcd__ n (m - n)

-- This allows for calling `show_person` with additional fields.
-- **row polymorphism**
show_person :: forall r. { first :: String, last :: String | r } -> String
-- This doesn't allow additional fields.
-- show_person :: { first :: String, last :: String } -> String
show_person { first: x, last: y } = y <> ", " <> x

sort_pair :: Array Int -> Array Int
sort_pair arr @ [x, y] -- named pattern, maybe this can be used like an `ActivePattern`?
    | x <= y = arr
    | otherwise = [y, x]
sort_pair arr = arr

lzs :: Array Int -> Array Int
lzs [] = []
lzs xs =
    case sum xs of
        0 -> xs
        _ -> lzs (fromMaybe [] $ tail xs)

data Shape
    = Circle Point Number
    | Rectangle Point Number Number
    | Line Point Point
    | Text Point String

data Point = Point {
    x :: Number,
    y :: Number
}

ex_line :: Shape
ex_line = Line p1 p2
    where
        p1 :: Point
        p1 = Point { x: 0.0, y: 0.0 }

        p2 :: Point
        p2 = Point { x: 10.0, y: 5.0 }

show_point :: Point -> String
show_point (Point { x, y }) = "(" <> show x <> ", " <> show y <> ")"

origin :: Point
origin = Point { x, y }
    where
        x = 0.0
        y = 0.0

-- Forced type aliasing.
newtype Pixels = Pixels Number
newtype Inches = Inches Number

-- Typeclasses are like Rust traits.
class Storage a where
    insert :: Array a -> a -> Array a

-- `SimpleStorage` extends `Storage`.
class Storage s <= SimpleStorage s where
    empty :: s

newtype Complex = Complex {
    real :: Number,
    imaginary :: Number
}

instance show_complex :: Show Complex where
    show (Complex c) = show c.real <> "*" <> show c.imaginary <> "i"

instance eq_complex :: Eq Complex where
    eq (Complex c) (Complex d) = c.real == d.real && c.imaginary == d.imaginary

show_compare :: forall a. Ord a => Show a => a -> a -> String
show_compare a1 a2
    | a1 < a2 = show a1 <> " is less than " <> show a2
    | a1 > a2 = show a1 <> " is greater than " <> show a2
show_compare a1 a2 = show a1 <> " is equal to " <> show a2

newtype HashCode = HashCode Int

hash_code :: Int -> HashCode
hash_code h = HashCode (mod h 65535)

class Eq a <= Hashable a where
    hash :: a -> HashCode

-- This needs to be added otherwise we get an error on `hash_equal` not being able to identify
-- which `eq` function to use on our type.
derive newtype instance eq_hash_code :: Eq HashCode

combine_hashes :: HashCode -> HashCode -> HashCode
combine_hashes (HashCode h1) (HashCode h2) = hash_code (73 * h1 + 51 * h2)

hash_equal :: forall a. Hashable a => a -> a -> Boolean
hash_equal = on eq hash

instance hash_int :: Hashable Int where
    hash = hash_code
