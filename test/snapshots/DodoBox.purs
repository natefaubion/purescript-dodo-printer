module  DodoBox where

import Prelude

import Ansi.Codes (GraphicsParam)
import Data.Array (intersperse)
import Dodo (Doc, plainText, print, textParagraph, twoSpaces)
import Dodo as Dodo
import Dodo.Ansi (ansiGraphics)
import Dodo.Ansi as Ansi
import Dodo.Box (Align(..), DocBox, docBox, fill, halign, horizontal, hpadding, sizeOf, valign, vertical)
import Dodo.Box as Box
import DodoExampleJson (exampleJson, printJson)
import Effect (Effect)
import Effect.Class.Console as Console

para1 :: forall a. Doc a
para1 = textParagraph
  """
  Quisque finibus tellus non molestie porta. In non posuere metus, vitae
  tincidunt enim. Nam quis elit pharetra, elementum elit lacinia, efficitur
  nibh. Cras lobortis neque sed ante ornare rutrum. Maecenas sed urna nisl.
  Phasellus aliquam finibus ex vitae iaculis. Vestibulum ante ipsum primis
  in faucibus orci luctus et ultrices posuere cubilia curae; Suspendisse
  eget tortor eget sapien tincidunt vestibulum eu a velit. Pellentesque eu
  tortor ut lectus sodales ornare.
  """

para2 :: forall a. Doc a
para2 = textParagraph
  """
  Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere
  cubilia curae; Suspendisse eget tortor eget sapien tincidunt vestibulum eu a
  velit. Pellentesque eu tortor ut lectus sodales ornare.
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
    ( horizontal $ intersperse (hpadding 3)
        [ textBox 40 (printJson exampleJson)
        , valign Middle $ vertical
            [ halign Middle $ textBox 40 (Ansi.bold (Dodo.text "NOTE"))
            , textBox 40 (Ansi.italic para2)
            ]
        ]
    )


main :: Effect Unit
main = do
  Console.log $ print plainText (twoSpaces { pageWidth = top }) test
  Console.log $ print ansiGraphics (twoSpaces { pageWidth = top }) test
