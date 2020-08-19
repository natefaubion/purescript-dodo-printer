module DodoLongLineSmallWidth where

import Prelude

import Dodo (Doc, foldWithSeparator, indent, plainText, print, text, twoSpaces)
import Dodo.Common (leadingComma, pursSquares)
import Effect (Effect)
import Effect.Class.Console as Console

test1 :: forall a. Doc a
test1 = do
  let array = pursSquares <<< foldWithSeparator leadingComma
  array
    [ indent $ array
        [ indent $ array []
        , indent $ array [ indent $ text "0" ]
        , indent $ array [ indent $ text "0" ]
        , indent $ array [ indent $ text "11111111111111111111111111111111111111111111111111111111111111111111" ]
        ]
    ]

main :: Effect Unit
main = do
  Console.log $ print plainText (twoSpaces { pageWidth = 200 }) test1
  Console.log $ print plainText (twoSpaces { pageWidth = 20 }) test1
