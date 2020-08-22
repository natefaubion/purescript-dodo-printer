module DodoRibbonRatio where

import Prelude

import Dodo (Doc, fourSpaces, indent, lines, plainText, print, textParagraph)
import Effect (Effect)
import Effect.Class.Console as Console

test :: forall a. Doc a
test = lines
  [ latin
  , indent $ indent latin
  , indent $ indent $ indent $ indent latin
  , indent $ indent $ indent $ indent $ indent $ indent latin
  ]
  where
  latin = textParagraph
    """
    Quisque finibus tellus non molestie porta. In non posuere metus, vitae
    tincidunt enim. Nam quis elit pharetra, elementum elit lacinia, efficitur
    nibh. Cras lobortis neque sed ante ornare rutrum. Maecenas sed urna nisl.
    Phasellus aliquam finibus ex vitae iaculis. Vestibulum ante ipsum primis
    in faucibus orci luctus et ultrices posuere cubilia curae; Suspendisse
    eget tortor eget sapien tincidunt vestibulum eu a velit. Pellentesque eu
    tortor ut lectus sodales ornare.
    """

main :: Effect Unit
main = do
  Console.log $ print plainText (fourSpaces { pageWidth = 160, ribbonRatio = 0.5 }) test
