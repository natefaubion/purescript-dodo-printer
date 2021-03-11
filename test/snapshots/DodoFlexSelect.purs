module DodoFlexSelect where

import Prelude

import Dodo (Doc, break, flexAlt, flexSelect, fourSpaces, indent, plainText, print, text, words)
import Effect (Effect)
import Effect.Class.Console as Console

test1 :: forall a. Doc a
test1 = words
  [ text "hello"
  , indent $ flexSelect
      (flexAlt (text "true") (text "false"))
      (break <> text "first")
      (break <> text "second")
  ]

test2 :: forall a. Doc a
test2 = words
  [ text "hello"
  , indent $ flexSelect
      (text "whatever")
      (break <> text "first")
      (break <> text "second")
  ]

main :: Effect Unit
main = do
  Console.log $ print plainText fourSpaces test1
  Console.log $ print plainText (fourSpaces { pageWidth = 1 }) test1
  Console.log $ print plainText fourSpaces test2
  Console.log $ print plainText (fourSpaces { pageWidth = 1 }) test2
