module FileOperations where

import Prelude

import Control.MonadZero (guard)
import Data.Array (concatMap, filter, foldl, foldr, head, null, tail, (..))
import Data.Foldable (product)
import Data.List.Lazy (foldrLazy)
import Data.List.Lazy.NonEmpty (length)
import Data.Maybe (fromMaybe)

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

length_ :: forall a. Array a -> Int
length_ arr =
    if null arr then
        0
    else
        1 + (length_ $ fromMaybe [] $ tail arr)

is_even :: Int -> Boolean
is_even 0 = true
is_even 1 = false
is_even n = is_even (n - 2)

add_1 :: Array Int -> Array Int
-- add_1 ns = map (\n -> n + 1) ns
add_1 ns = (\n -> n + 1) <$> ns

squares :: Array Int -> Array Int
-- squares = map (\n -> n * n)
squares ns = (\n -> n * n) <$> ns

no_negatives :: Array Int -> Array Int
no_negatives = filter (\n -> n > 0)

infix 8 filter as <$?>

no_negatives_ :: Array Int -> Array Int
no_negatives_ ns = (\n -> n > 0) <$?> ns

factors :: Int -> Array (Array Int)
-- factors n = filter (\xs -> product xs == n) $ do
factors n = do
    i <- 1 .. n
    j <- i .. n
    guard $ i * j == n
    -- [[i, j]]
    pure [i, j]

left :: String
left = foldl (\acc n -> acc <> show n) "" [1,2,3,4,5]
-- "12345"

right :: String
right = foldr (\n acc -> acc <> show n) "" [1,2,3,4,5]
-- "54321"

not_tail_add_is_last :: Int -> Int
not_tail_add_is_last 0 = 0
not_tail_add_is_last n = 1 + not_tail_add_is_last (n - 1)

tail_rec :: Int -> Int
tail_rec 0 = 0
tail_rec n = tail_rec (n - 1)

len :: forall a. Array a -> Int -> Int
len [] _ = 0
len arr acc = len (fromMaybe [] $ tail arr) (acc + 1)
-- WARNING(alex): This doesn't trigger TCO!!!
-- len arr acc = len (fromMaybe [] $ tail arr) acc + 1

length_tail :: forall a. Array a -> Int
length_tail arr = len_ arr 0
    where
        len_ :: forall a. Array a -> Int -> Int
        len_ arr acc =
            if null arr then
                0
            else
                len_ (fromMaybe [] $ tail arr) (acc + 1)

-- all_files :: Path -> Array Path
-- all_files file = file : concatMap all_files (ls file)
