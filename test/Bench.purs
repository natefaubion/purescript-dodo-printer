module Test.Bench where

import Prelude

import Data.Monoid (power)
import Dodo (Doc, align, break, plainText, print, text, textParagraph, twoSpaces)
import Effect (Effect)
import Performance.Minibench (benchWith)

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
  benchWith 3000 \_ -> print plainText twoSpaces (power test 10)
