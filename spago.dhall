{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "dodo-printer"
, dependencies =
  [ "aff"
  , "ansi"
  , "avar"
  , "console"
  , "effect"
  , "foldable-traversable"
  , "lists"
  , "maybe"
  , "node-child-process"
  , "node-fs-aff"
  , "node-process"
  , "psci-support"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
