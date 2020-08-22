module DodoWithPosition where

import Prelude

import Data.Foldable (intercalate)
import Dodo (Doc, align, paragraph, plainText, print, text, twoSpaces, withPosition)
import Effect (Effect)
import Effect.Class.Console as Console

test1 :: forall a. Doc a
test1 =
  paragraph
    [ text "abcedefg"
    , align 5 $ withPosition \pos ->
        text $ intercalate " "
          [ show pos.line
          , show pos.column
          , show pos.indent
          , show pos.ribbonWidth
          , show pos.pageWidth
          ]
    ]

main :: Effect Unit
main = do
  Console.log $ print plainText (twoSpaces { ribbonRatio = 0.5 }) test1
  Console.log $ print plainText (twoSpaces { ribbonRatio = 0.5, pageWidth = 10 }) test1
