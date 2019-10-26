module Chapter7 where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

-- instance functor_maybe :: Functor Maybe where
    -- map f (Just a) = Just (f a)
    -- map f Nothing = Nothing

-- instance apply_maybe :: Apply Maybe where
    -- apply (Just f) (Just x) = Just (f x)
    -- apply _ _ = Nothing

type Position = {
    x :: Int,
    y :: Int
}

make_position :: Int -> Int -> Position
make_position x y = { x, y }

-- <$> map
-- <*> apply
lifting :: Maybe Position
-- lifting = make_position <$> Nothing <*> Just 11
-- lifting = apply (map make_position (Just 10)) (Just 11)
lifting = ado
    x <- Just 10
    y <- Just 11
    in make_position x y

with_error :: forall a b. Maybe a -> b -> Either b a
with_error Nothing err = Left err
with_error (Just a) _ = Right a

-- WARNING(alex): `Either` defies the convention of `Ok` being the first type parameter, and
-- `Error` being the second.
-- Pay attention when trying to match, if this was F# or Rust, the declaration would be:
-- eithering :: Maybe Int -> Maybe Int -> Result Position String
-- But in purescript it's reversed. Remember who was right and who was left!
eithering :: Maybe Int -> Maybe Int -> Either String Position
eithering a b = make_position <$> (with_error a "Missing (x).") <*> (with_error b "Missing (y).")
-- eithering a b = ado
    -- x <- a `with_error` "Missing x"
    -- y <- b `with_error` "Missing y"
    -- in make_position x y
