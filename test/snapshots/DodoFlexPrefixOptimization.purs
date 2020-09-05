module DodoFlexPrefixOptimization where

import Prelude

import Data.Foldable (fold)
import Dodo (Doc, align, flexAlt, flexGroup, fourSpaces, paragraph, plainText, print, text, withPosition)
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Unsafe (unsafePerformEffect)

logText :: forall a. String -> Doc a
logText str = withPosition \_ ->
  unsafePerformEffect do
    Console.log $ "out: " <> str
    pure $ text str

test1 :: forall a. Doc a
test1 = flexGroup $ fold
  [ logText "a"
  , logText "b"
  , logText "c"
  , flexAlt (logText "e") (logText "d")
  ]

test2 :: forall a. Doc a
test2 = flexGroup $ fold
  [ logText "a"
  , flexGroup $ fold
      [ logText "b"
      , flexGroup $ fold
          [ logText "c"
          , flexAlt (logText "e") (logText "d")
          ]
      ]
  ]

-- Bad output due to aggressive optimization:
-- * a
--   b c d e
--   f g
test3 :: forall a. Doc a
test3 = flexGroup $ text "* " <> align 2 letters
  where
  letters =
    paragraph $ map logText
      [ "a"
      , "b"
      , "c"
      , "d"
      , "e"
      , "f"
      , "g"
      ]

main :: Effect Unit
main = do
  Console.log $ print plainText (fourSpaces { pageWidth = 1 }) test1
  Console.log $ print plainText fourSpaces test1
  Console.log $ print plainText (fourSpaces { pageWidth = 1 }) test2
  Console.log $ print plainText fourSpaces test2
  Console.log $ print plainText (fourSpaces { pageWidth = 10 }) test3
