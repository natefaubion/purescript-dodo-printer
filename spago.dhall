{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "dodo-printer"
, dependencies =
  [ "aff"
  , "ansi"
  , "arrays"
  , "avar"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "integers"
  , "lists"
  , "maybe"
  , "minibench"
  , "node-buffer"
  , "node-child-process"
  , "node-fs-aff"
  , "node-path"
  , "node-process"
  , "node-streams"
  , "parallel"
  , "posix-types"
  , "prelude"
  , "psci-support"
  , "strings"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
