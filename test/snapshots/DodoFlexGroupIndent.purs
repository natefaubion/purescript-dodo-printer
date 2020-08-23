module DodoFlexGroupIndent where

import Prelude

import Dodo (Doc, align, flexGroup, fourSpaces, indent, paragraph, plainText, print, text)
import Effect (Effect)
import Effect.Class.Console as Console

test1 :: forall a. Doc a
test1 = flexGroup $ indent $ paragraph
  [ text "a"
  , text "b"
  , text "c"
  ]

test2 :: forall a. Doc a
test2 = flexGroup $ align 20 $ paragraph
  [ text "a"
  , text "b"
  , text "c"
  ]

main :: Effect Unit
main = do
  Console.log $ print plainText fourSpaces test1
  Console.log $ print plainText (fourSpaces { pageWidth = 0 }) test1
  Console.log $ print plainText fourSpaces test2
  Console.log $ print plainText (fourSpaces { pageWidth = 0 }) test2
