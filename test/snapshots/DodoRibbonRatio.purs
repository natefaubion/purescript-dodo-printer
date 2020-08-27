module DodoRibbonRatio where

import Prelude

import Data.Foldable (foldl)
import Data.Int as Int
import Data.Monoid (power)
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits as SCU
import Dodo (Doc, flexGroup, fourSpaces, indent, lines, plainText, print, softBreak, text, withPosition)
import Effect (Effect)
import Effect.Class.Console as Console

pageArea :: forall a. Doc a
pageArea = withPosition \pos -> do
  let restWidth = pos.pageWidth - pos.ribbonWidth - pos.indent
  let printable = SCU.take pos.ribbonWidth $ power "1234567890" $ Int.ceil (Int.toNumber pos.ribbonWidth / 10.0)
  let rest = power "-" restWidth
  text (printable <> rest)

test :: forall a. Doc a
test = lines
  [ chunk
  , indent chunk
  , indent $ indent chunk
  , indent $ indent $ indent chunk
  , indent $ indent $ indent $ indent chunk
  ]
  where
  chunk =
    lines [ pageArea, letters ]

  letters =
    foldl appendSoftBreak mempty
      $ map text
      $ String.split (Pattern "")
      $ "abcdefghijklmnopqrstuvwxyz"

  appendSoftBreak a b =
    a <> flexGroup (softBreak <> b)

main :: Effect Unit
main = do
  Console.log $ print plainText (fourSpaces { pageWidth = 20, ribbonRatio = 0.5 }) test
  Console.log $ print plainText (fourSpaces { pageWidth = 40, ribbonRatio = 0.75 }) test
