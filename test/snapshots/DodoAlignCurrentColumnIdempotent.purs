module DodoAlignCurrentColumnIdempotent where

import Prelude

import Dodo (Doc, alignCurrentColumn, flexGroup, fourSpaces, plainText, print, softBreak, text)
import Effect (Effect)
import Effect.Class.Console as Console

test :: forall a. Doc a
test = text "1234" <> aligns
  where
  aligns = flexGroup
    $ alignCurrentColumn
    $ alignCurrentColumn
    $ alignCurrentColumn
    $ softBreak <> text "1234"

main :: Effect Unit
main = Console.log $ print plainText (fourSpaces { pageWidth = 0 }) test
