{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "arrays"
    , "console"
    , "control"
    , "effect"
    , "foldable-traversable"
    , "globals"
    , "lists"
    , "math"
    , "maybe"
    , "psci-support"
    , "strings"
    , "validation"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
