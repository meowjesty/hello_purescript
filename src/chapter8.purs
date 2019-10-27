module Chapter8 where

import Prelude

import Control.Plus (empty)
import Data.Array (head, tail, (..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Effect.Random (random)

count_throws :: Int -> Array (Array Int)
count_throws n = do
    x <- 1 .. 6
    y <- 1 .. 6
    if x + y == n then
        pure [x, y]
    else
        empty

foldM_ :: forall m a b. Monad m => (a -> b -> m a) -> a -> List b -> m a
foldM_ _ a Nil = pure a
foldM_ f a (b : bs) = do
    a' <- f a b     -- a' is the new accumulator / b is the value
    foldM_ f a' bs  -- pass new accumulator

safe_divide :: Int -> Int -> Maybe Int
safe_divide _ 0 = Nothing
safe_divide a b = Just (a / b)

third :: forall a. Array a -> Maybe a
third [] = Nothing
third xs = do
    uno <- tail xs
    dos <- tail uno
    tres <- head dos
    pure tres

rander :: Effect Unit
rander = do
    n <- random
    logShow n

print_rand_1 :: Aff Unit
print_rand_1 = liftEffect do_random
    where
        do_random :: Effect Unit
        do_random = do
            n <- random
            logShow n

print_rand_2 :: Aff Unit
print_rand_2 = liftEffect $ do
    n <- random
    logShow n

print_rand_3 :: Aff Unit
print_rand_3 = do
    n <- liftEffect random
    liftEffect $ logShow n

print_rand_4 :: Aff Unit
print_rand_4 = do
    n <- random # liftEffect
    (logShow n) # liftEffect

