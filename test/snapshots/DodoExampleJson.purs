module DodoExampleJson where

import Prelude

import Ansi.Codes (Color(..), GraphicsParam)
import Data.Foldable (fold)
import Data.Tuple (Tuple(..))
import Dodo (Doc, foldWithSeparator, plainText, print, text, twoSpaces)
import Dodo.Ansi (ansiGraphics, dim, foreground)
import Dodo.Common (jsCurlies, jsSquares, trailingComma)
import Effect (Effect)
import Effect.Class.Console as Console

data Json
  = JNull
  | JString String
  | JNumber Number
  | JBool Boolean
  | JArray (Array Json)
  | JObject (Array (Tuple String Json))

printJson :: Json -> Doc GraphicsParam
printJson = case _ of
  JNull -> dim $ text "null"
  JString str -> foreground Red $ text $ show str
  JNumber num -> foreground Green $ text $ show num
  JBool bool -> foreground Blue $ text $ show bool
  JArray arr -> jsSquares $ foldWithSeparator trailingComma $ map printJson arr
  JObject obj -> jsCurlies $ foldWithSeparator trailingComma $ map printKeyValue obj
  where
  printKeyValue (Tuple key value) =
    fold
      [ foreground Yellow $ text $ show key
      , text ": "
      , printJson value
      ]

infix 0 Tuple as :

exampleJson :: Json
exampleJson =
  JObject
    [ "bool": JBool true
    , "string": JString "bar"
    , "number": JNumber 1234.0
    , "array": JArray
        [ JNull
        , JString "two"
        , JArray [ JString "three"]
        ]
    , "object": JObject
        []
    , "wideObject": JObject
        [ "key": JString "1111111111111111111111111111111111111111"
        ]
    ]

main :: Effect Unit
main = do
  let json = printJson exampleJson
  Console.log $ print plainText (twoSpaces { pageWidth = 200 }) json
  Console.log $ print ansiGraphics (twoSpaces { pageWidth = 200 }) json
  Console.log $ print plainText (twoSpaces { pageWidth = 80 }) json
  Console.log $ print ansiGraphics (twoSpaces { pageWidth = 80 }) json
  Console.log $ print plainText (twoSpaces { pageWidth = 20 }) json
  Console.log $ print ansiGraphics (twoSpaces { pageWidth = 20 }) json
