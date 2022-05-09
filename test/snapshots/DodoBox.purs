module DodoBox where

import Prelude

import Ansi.Codes (GraphicsParam)
import Data.Array (intersperse)
import Data.Array as Array
import Data.Maybe (fromMaybe)
import Dodo (Doc, plainText, print, textParagraph, twoSpaces)
import Dodo as Dodo
import Dodo.Ansi (ansiGraphics)
import Dodo.Ansi as Ansi
import Dodo.Box (Align(..), DocBox, docBox, fill, halign, horizontal, hpadding, resize, sizeOf, valign, vertical)
import Dodo.Box as Box
import DodoExampleJson (exampleJson, printJson)
import Effect (Effect)
import Effect.Class.Console as Console

para2 :: forall a. Doc a
para2 = textParagraph
  """
  Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere
  cubilia curae; Suspendisse eget tortor.
  """

textBox :: forall a. Int -> Doc a -> DocBox a
textBox pageWidth = print docBox (twoSpaces { pageWidth = pageWidth })

heading :: DocBox GraphicsParam -> DocBox GraphicsParam -> DocBox GraphicsParam
heading head body =
  vertical
    [ head
    , fill (Ansi.dim (Dodo.text "-"))
        { width: max (sizeOf head).width (sizeOf body).width
        , height: 1
        }
    , body
    ]

test :: Doc GraphicsParam
test = Box.toDoc do
  heading
    (textBox 40 (Ansi.bold (Dodo.text "Example JSON")))
    ( vertical
        [ fill (Ansi.dim (Dodo.text "*")) { width: 120, height: 1 }
        , halign Middle $ horizontal $ intersperse (hpadding 4)
            [ textBox 40 (printJson exampleJson)
            , valign Middle $ vertical
                [ halign Middle $ textBox 40 (Ansi.bold (Dodo.text "NOTE"))
                , textBox 40 (Ansi.italic para2)
                ]
            ]
        ]
    )

table
  :: forall a
   . { headers :: Array (DocBox a)
     , rows :: Array (Array (DocBox a))
     }
  -> DocBox a
table { headers, rows } =
  vertical
    [ rowSep
    , vertical $ Array.intersperse rowSep $ map columns $ Array.cons headers rows
    , rowSep
    ]
  where
  joint =
    fill (Dodo.text "+") { width: 1, height: 1 }

  rowSep =
    horizontal
      [ joint
      , horizontal $ Array.intersperse joint $ map
          ( \width ->
              fill (Dodo.text "-")
                { width: width + 2
                , height: 1
                }
          )
          widths
      , joint
      ]

  columns cols = do
    let
      height =
        Array.foldr (max <<< _.height <<< Box.sizeOf) 0 cols

      colBoxes = Array.mapWithIndex
        ( \ix col ->
            horizontal
              [ hpadding 1
              , resize
                  { width: fromMaybe 0 (Array.index widths ix)
                  , height
                  }
                  col
              , hpadding 1
              ]
        )
        cols

      sep = fill (Dodo.text "|") { width: 1, height }

    horizontal
      [ sep
      , horizontal $ Array.intersperse sep colBoxes
      , sep
      ]

  widths = Array.mapWithIndex
    ( \ix hd ->
        Array.foldr
          ( flip Array.index ix
              >>> map (_.width <<< Box.sizeOf)
              >>> fromMaybe 0
              >>> max
          )
          (Box.sizeOf hd).width
          rows
    )
    headers

testTable :: Doc GraphicsParam
testTable = Box.toDoc $ table
  { headers:
      [ valign Middle $ halign Middle $ textBox 20 $ Dodo.text "Example"
      , valign Middle $ halign Middle $ textBox 20 $ Dodo.text "Comment"
      ]
  , rows:
      [ [ textBox 40 (printJson exampleJson)
        , valign Middle $ halign Middle $ textBox 40 (Ansi.italic para2)
        ]
      , [ textBox 120 (printJson exampleJson)
        , valign Middle $ textBox 60 (Ansi.italic para2)
        ]
      , [ textBox 120 (printJson exampleJson)
        , textBox 60 (Ansi.italic para2)
        ]
      ]
  }

main :: Effect Unit
main = do
  Console.log $ print plainText (twoSpaces { pageWidth = top }) test
  Console.log $ print ansiGraphics (twoSpaces { pageWidth = top }) test
  Console.log $ print plainText (twoSpaces { pageWidth = top }) testTable
