module DodoLocally where

import Prelude

import Dodo (Doc, fourSpaces, indent, lines, locally, plainText, print, text)
import Effect (Effect)
import Effect.Class.Console as Console

test1 :: forall a. Doc a
test1 = lines
  [ text "hello"
  , indent $ lines
      [ text "indented"
      , locally (_ { indent = 2, indentSpaces = "  " }) do
          lines
            [ text "undented"
            , indent $ text "undented indent"
            ]
      , text "indented again"
      ]
  ]

main :: Effect Unit
main = do
  Console.log $ print plainText fourSpaces test1
