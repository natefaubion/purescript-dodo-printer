module DodoFlexSelectHanging where

import Prelude

import Data.Foldable (fold, foldr)
import Data.Tuple (Tuple(..), fst, snd)
import Dodo (Doc, break, flexGroup, flexSelect, indent, plainText, print, space, spaceBreak, text, twoSpaces)
import Dodo.Common (pursCurlies)
import Effect (Effect)
import Effect.Class.Console as Console

data Hanging a
  = NoHang (Doc a)
  | Hang (Doc a) (Doc a)

arg1 :: forall a. Hanging a
arg1 =
  Hang (text "bar") $ pursCurlies $ flexGroup $ fold
    [ text "prop"
    , space
    , text "="
    , spaceBreak
    , indent $ text "42"
    ]

arg2 :: forall a. Hanging a
arg2 = NoHang $ text "baz"

arg3 :: forall a. Hanging a
arg3 = Hang (text "\\a ->") (text "a")

joinArgs :: forall a. Array (Hanging a) -> Hanging a -> Doc a
joinArgs args last = fst $ foldr go start args
  where
  start = case last of
    Hang a b ->
      Tuple
        (flexSelect
          (spaceBreak <> a)
          (flexGroup (spaceBreak <> b))
          (flexGroup (spaceBreak <> indent b)))
        (break <> a <> flexGroup (spaceBreak <> indent b))
    NoHang a ->
      Tuple
        (flexGroup (spaceBreak <> a))
        (break <> a)

  go hdoc next = do
    let
      doc = case hdoc of
        NoHang a ->
          a
        Hang a b ->
          a <> flexGroup (spaceBreak <> indent b)
    Tuple
      (flexSelect
        (spaceBreak <> doc)
        (fst next)
        (snd next))
      (break <> doc <> snd next)

test :: forall a. Doc a
test = text "foo" <> indent (joinArgs [ arg1, arg2 ] arg3)

main :: Effect Unit
main = do
  Console.log $ print plainText twoSpaces test
  Console.log $ print plainText (twoSpaces { pageWidth = 32 }) test
  Console.log $ print plainText (twoSpaces { pageWidth = 24 }) test
  Console.log $ print plainText (twoSpaces { pageWidth = 20 }) test
  Console.log $ print plainText (twoSpaces { pageWidth = 16 }) test
  Console.log $ print plainText (twoSpaces { pageWidth = 10 }) test
  Console.log $ print plainText (twoSpaces { pageWidth = 1 }) test
