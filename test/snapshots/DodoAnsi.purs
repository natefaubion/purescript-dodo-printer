module DodoAnsi where

import Prelude

import Dodo (Doc, indent, lines, print, text, twoSpaces, words)
import Dodo.Ansi as Ansi
import Effect (Effect)
import Effect.Class.Console as Console

test1 :: Doc Ansi.GraphicsParam
test1 =
  Ansi.foreground Ansi.Red $ Ansi.strikethrough $ words
    [ text "This is"
    , Ansi.foreground Ansi.Blue $ Ansi.bold $ text "bold"
    , Ansi.reset $ Ansi.underline $ text "text."
    , text "The end."
    ]

test2 :: Doc Ansi.GraphicsParam
test2 =
  lines
    [ text "Line with no style"
    , Ansi.background Ansi.Yellow $ Ansi.bold $ lines
        [ Ansi.strikethrough $ text "Strikethrough"
        , indent $ Ansi.foreground Ansi.Red $ lines
            [ text "Red"
            , Ansi.reset $ text "Lines"
            , text "And bold"
            ]
        , text "The end."
        ]
    ]

main :: Effect Unit
main = do
  Console.log $ print Ansi.ansiGraphics twoSpaces test1
  Console.log $ print Ansi.ansiGraphics twoSpaces test2
