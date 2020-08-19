module DodoTextParagraph where

import Prelude

import Dodo (Doc, align, break, plainText, print, text, textParagraph, twoSpaces)
import Effect (Effect)
import Effect.Class.Console as Console

test :: forall a. Doc a
test = point latin <> break
  where
  point = append (text "* ") <<< align 2 <<< textParagraph
  latin =
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
  Console.log $ print plainText (twoSpaces { pageWidth = 160 }) test
  Console.log $ print plainText (twoSpaces { pageWidth = 80 }) test
  Console.log $ print plainText (twoSpaces { pageWidth = 40 }) test
