-- Modules accept `.` like F# namespaces.
-- module UI.Client.Main
module Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (logShow)
import Math (pi, sqrt)

data Box a = Box a

boxed_int :: Box Int
boxed_int = Box 4

function_no_sugar :: Function Int Int
function_no_sugar = (\x -> x + 2)

function_with_sugar :: (Int -> Int)
function_with_sugar = (\x -> x + 2)

-- F# would turn this into a property, here it's the same as a static value.
literal_value :: String
literal_value = "literal value"

two_arg_fn :: Int -> Int -> Int
two_arg_fn a1 a2 = a1 + a2

inline_syntax :: (Int -> Int -> Int)
inline_syntax = (\x y-> x + y)

fn_takes_fn :: Int -> (Int -> Int) -> Int
fn_takes_fn val fn = fn val

fn_returns_fn :: Int -> (Int -> Int)
fn_returns_fn val = (\x -> val + x)

-- has some problems
-- https://github.com/purescript/purescript/issues/950
abbreviated :: Int -> String
abbreviated = show

use_abbreviated :: Boolean
use_abbreviated = (abbreviated 4) == "4"

data Singleton = Singleton

data SingletonArgs = SingletonArgs Int String

data Variant
    = Thing Int
    | Nothing
    | TwoThing Int Int

data Generics a b
    = StoreA a
    | StoreB b
    | StoresAB a b

data NoImpl

diagonal w h = sqrt (w * w + h * h)

circle_area :: Number -> Number
circle_area r = 2.0 * pi * r

main :: Effect Unit
main = do
    logShow (diagonal 3.0 4.0)
