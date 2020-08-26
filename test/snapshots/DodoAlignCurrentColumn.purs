module DodoAlignCurrentColumn where

import Prelude

import Dodo (Doc, alignCurrentColumn, fourSpaces, lines, plainText, print, text, textParagraph, words)
import Effect (Effect)
import Effect.Class.Console as Console

test1 :: forall a. Doc a
test1 = lines
  [ words [ text "111", hanging ]
  , words [ text "111111", hanging ]
  , words [ text "111111111", hanging ]
  ]
  where
  hanging = alignCurrentColumn $ textParagraph "a b c d e f g"

main :: Effect Unit
main = do
  Console.log $ print plainText (fourSpaces { pageWidth = 12 }) test1
