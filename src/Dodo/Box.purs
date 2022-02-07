module Dodo.Box
  ( DocBox
  , DocBoxBuffer
  , DocAnnStk
  , Vertical(..)
  , Horizontal(..)
  , Align(..)
  , BoxSize
  , valign
  , halign
  , vappend
  , happend
  , vertical
  , verticalWithAlign
  , horizontal
  , horizontalWithAlign
  , fill
  , vpadding
  , hpadding
  , sizeOf
  , isEmpty
  , empty
  , toDoc
  , docBox
  ) where

import Prelude

import Data.Either (Either(..), isLeft)
import Data.Foldable (class Foldable, foldl, foldr)
import Data.Int as Int
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.Newtype (class Newtype, un, under)
import Dodo (Doc, Printer(..), annotate)
import Dodo as Dodo
import Dodo.Internal as Internal
import Partial.Unsafe (unsafeCrashWith)
import Safe.Coerce (coerce)

data Align
  = Start
  | Middle
  | End

derive instance Eq Align

type BoxSize =
  { width :: Int
  , height :: Int
  }

-- | Unlike `Doc`s, which can only be joined along a line, `DocBox`es
-- | are two-dimensional units which can be stacked vertically and
-- | horizontally (eg. for tables).
-- |
-- | `Doc`s can be lifted into `DocBox` by using the `doxBox` printer.
-- | ```purescript
-- | example = Dodo.print docBox twoSpaces myDoc
-- | ````
data DocBox a
  = DocLine (Doc a) Int
  | DocVApp (DocBox a) (DocBox a) BoxSize
  | DocHApp (DocBox a) (DocBox a) BoxSize
  | DocAlign Align Align (DocBox a)
  | DocEmpty

derive instance Functor DocBox

-- | A newtype whose Semigroup instance stacks DocBoxes vertically.
newtype Vertical a = Vertical (DocBox a)

derive instance Newtype (Vertical a) _

derive newtype instance Functor Vertical

instance Semigroup (Vertical a) where
  append = coerce (vappend :: DocBox a -> _ -> _)

instance Monoid (Vertical a) where
  mempty = Vertical DocEmpty

-- | A newtype whose Semigroup instance stacks DocBoxes horizontally.
newtype Horizontal a = Horizontal (DocBox a)

derive instance Newtype (Horizontal a) _

derive newtype instance Functor Horizontal

instance Semigroup (Horizontal a) where
  append = coerce (happend :: DocBox a -> _ -> _)

instance Monoid (Horizontal a) where
  mempty = Horizontal DocEmpty

-- | Joins DocBoxes in a vertical run.
vertical :: forall f a. Foldable f => f (DocBox a) -> DocBox a
vertical = foldr vappend DocEmpty

-- | Joins DocBoxes in a vertical run with uniform horizontal alignment.
verticalWithAlign :: forall f a. Foldable f => Align -> f (DocBox a) -> DocBox a
verticalWithAlign align = foldr (\a b -> halign align a `vappend` b) DocEmpty

-- | Joins DocBoxes in a horizontal run.
horizontal :: forall f a. Foldable f => f (DocBox a) -> DocBox a
horizontal = foldr happend DocEmpty

-- | Joins DocBoxes in a horizontal run with uniform vertical alignment.
horizontalWithAlign :: forall f a. Foldable f => Align -> f (DocBox a) -> DocBox a
horizontalWithAlign align = foldr (\a b -> valign align a `happend` b) DocEmpty

-- | Joins two DocBoxes vertically on top of each other.
vappend :: forall a. DocBox a -> DocBox a -> DocBox a
vappend = case _, _ of
  DocEmpty, b -> b
  a, DocEmpty -> a
  a, b -> do
    let sizea = sizeOf a
    let sizeb = sizeOf b
    DocVApp a b
      { width: max sizea.width sizeb.width
      , height: sizea.height + sizeb.height
      }

-- | Joins two DocBoxes horizontally next to each other.
happend :: forall a. DocBox a -> DocBox a -> DocBox a
happend = case _, _ of
  DocEmpty, b -> b
  a, DocEmpty -> a
  a, b -> do
    let sizea = sizeOf a
    let sizeb = sizeOf b
    DocHApp a b
      { width: sizea.width + sizeb.width
      , height: max sizea.height sizeb.height
      }

-- | Pads a DocBox vertically to fit the tallest box within a
-- | horizontal run.
-- | ```purescript
-- | example =
-- |   valign Middle shortDoc
-- |     `happend` tallDoc
-- | ```
valign :: forall a. Align -> DocBox a -> DocBox a
valign a = case _ of
  DocAlign _ b doc
    | Start <- a, Start <- b -> doc
    | otherwise ->
        DocAlign a b doc
  other ->
    DocAlign a Start other

-- | Pads a DocBox horizontally to fit the widest line within a
-- | vertical run.
-- | ```purescript
-- | example =
-- |   halign Middle skinnyDoc
-- |     `vappend` wideDoc
-- | ```
halign :: forall a. Align -> DocBox a -> DocBox a
halign b = case _ of
  DocAlign a _ doc
    | Start <- a, Start <- b -> doc
    | otherwise ->
        DocAlign a b doc
  other ->
    DocAlign Start b other

-- | Fills a box to a given size with a Doc. The Doc is assumed
-- | to be 1x1. Providing a Doc of a different size will result
-- | in incorrect layouts.
-- | ```
-- | example =
-- |   fill (Ansi.dim (Dodo.text "-"))
-- |     { width: 100
-- |     , height: 1
-- |     }
-- | ```
fill :: forall a. Doc a -> BoxSize -> DocBox a
fill ch { width, height } = under Vertical (flip power height) line
  where
  line = case ch of
    Internal.Annotate a doc ->
      DocLine (annotate a (power doc width)) width
    _ ->
      DocLine (power ch width) width

-- | Vertical padding of a specific height.
vpadding :: forall a. Int -> DocBox a
vpadding = un Vertical <<< power (Vertical blank)

-- | Horizontal padding of a specific width.
hpadding :: forall a. Int -> DocBox a
hpadding = fill (Dodo.text " ") <<< { width: _, height: 1 }

-- | Returns the size of a DocBox.
sizeOf :: forall a. DocBox a -> BoxSize
sizeOf = case _ of
  DocLine _ width -> { width, height: 1 }
  DocVApp _ _ size -> size
  DocHApp _ _ size -> size
  DocAlign _ _ doc -> sizeOf doc
  DocEmpty -> { width: 0, height: 0 }

valignOf :: forall a. DocBox a -> Align
valignOf = case _ of
  DocAlign v _ _ -> v
  _ -> Start

-- | A blank line with 0 width.
blank :: forall a. DocBox a
blank = DocLine mempty 0

-- | The identity DocBox.
empty :: forall a. DocBox a
empty = DocEmpty

-- | Checks whether a DocBox is empty.
isEmpty :: forall a. DocBox a -> Boolean
isEmpty = case _ of
  DocEmpty -> true
  _ -> false

--  Converts a DocBox back into Doc for printing.
toDoc :: forall a. DocBox a -> Doc a
toDoc =
  go1
    <<< resume
    <<< (\b -> build (sizeOf b).width Start StpNil b)
  where
  go1 = case _ of
    Nothing -> mempty
    Just { doc, next } ->
      go2 doc (resume next)

  go2 acc = case _ of
    Nothing -> acc
    Just { doc, next } ->
      go2 (acc <> Dodo.break <> doc) (resume next)

data DocBoxStep a
  = StpNil
  | StpLine (Doc a) Int Int Align (DocBoxStep a)
  | StpVert (DocBox a) Int Align (DocBoxStep a)
  | StpHorz (List (ColStep a)) (DocBoxStep a)

type ColStep a =
  { fullWidth :: Int
  , next :: DocBoxStep a
  }

type DocColState a =
  { align :: Align
  , boxes :: List (DocBox a)
  , fullHeight :: Int
  , fullWidth :: Int
  , next :: DocBoxStep a
  , stack :: List (DocBox a)
  , steps :: List (ColStep a)
  }

type DocBuildState a =
  { align :: Align
  , columnsStack :: List (DocColState a)
  , fullWidth :: Int
  , next :: DocBoxStep a
  }

build :: forall a. Int -> Align -> DocBoxStep a -> DocBox a -> DocBoxStep a
build =
  ( \fullWidth align next box ->
      go (pure box)
        { align
        , columnsStack: List.Nil
        , fullWidth
        , next
        }
  )
  where
  go :: List (DocBox a) -> DocBuildState a -> DocBoxStep a
  go stack state = case stack of
    List.Nil ->
      case state.columnsStack of
        c : cs -> do
          let step = { fullWidth: state.fullWidth, next: state.next }
          let steps = step : c.steps
          case c.boxes of
            b : bs ->
              go (pure (under Vertical (padVertical c.fullHeight (valignOf b)) b))
                { align: Start
                , columnsStack:
                    List.Cons
                      { align: state.align
                      , boxes: bs
                      , fullHeight: c.fullHeight
                      , fullWidth: c.fullWidth
                      , next: c.next
                      , stack: c.stack
                      , steps
                      }
                      cs
                , fullWidth: (sizeOf b).width
                , next: StpNil
                }
            List.Nil ->
              go c.stack
                { align: c.align
                , columnsStack: cs
                , fullWidth: c.fullWidth
                , next: StpHorz steps c.next
                }
        List.Nil ->
          state.next
    box : stk ->
      case box of
        DocEmpty ->
          go stk state
        DocLine doc w ->
          go stk state
            { next = StpLine doc w state.fullWidth state.align state.next
            }
        DocVApp a b _ ->
          go (a : stk) state
            { next = (StpVert b state.fullWidth state.align state.next)
            }
        DocHApp _ _ _ -> do
          let fullHeight = (sizeOf box).height
          case flatten List.Nil (pure box) of
            b : boxes -> do
              go (pure (under Vertical (padVertical fullHeight (valignOf b)) b))
                { align: Start
                , columnsStack:
                    List.Cons
                      { align: state.align
                      , boxes
                      , fullHeight
                      , fullWidth: state.fullWidth
                      , next: state.next
                      , stack: stk
                      , steps: List.Nil
                      }
                      state.columnsStack
                , fullWidth: (sizeOf b).width
                , next: StpNil
                }
            List.Nil ->
              unsafeCrashWith "build: empty happend"
        DocAlign _ align doc ->
          go (doc : stk) state
            { align = align
            }

  flatten :: List (DocBox a) -> List (DocBox a) -> List (DocBox a)
  flatten acc = case _ of
    doc : rest ->
      case doc of
        DocHApp a b _ ->
          flatten acc (a : b : rest)
        _ ->
          flatten (doc : acc) rest
    List.Nil ->
      acc

  padVertical :: Int -> Align -> Vertical a -> Vertical a
  padVertical fullHeight align box@(Vertical b) = case align of
    Start -> box
    Middle -> do
      let mid = Int.toNumber (fullHeight - (sizeOf b).height) / 2.0
      power (Vertical blank) (Int.floor mid)
        <> box
        <> power (Vertical blank) (Int.ceil mid)
    End ->
      power (Vertical blank) (fullHeight - (sizeOf b).height)
        <> box

type DocGen a =
  { doc :: Doc a
  , width :: Int
  , next :: DocBoxStep a
  }

resume :: forall a. DocBoxStep a -> Maybe (DocGen a)
resume = case _ of
  StpNil ->
    Nothing
  StpLine doc width fullWidth align next ->
    case align of
      Start ->
        Just { doc, width, next }
      Middle -> do
        let mid = Int.toNumber (fullWidth - width) / 2.0
        Just
          { doc:
              power Dodo.space (Int.floor mid)
                <> doc
                <> power Dodo.space (Int.ceil mid)
          , width: fullWidth
          , next
          }
      End ->
        Just
          { doc:
              power Dodo.space (fullWidth - width)
                <> doc
          , width: fullWidth
          , next
          }
  StpVert b fullWidth align next ->
    resume (build fullWidth align next b)
  StpHorz columns next -> do
    let
      go doc gens width done = case _ of
        List.Nil
          | done -> Nothing
          | otherwise ->
              Just
                { doc
                , width
                , next: StpHorz (List.reverse gens) next
                }
        c : cs ->
          case resume c.next of
            Just gen -> do
              let doc' = doc <> gen.doc <> power Dodo.space (max 0 (c.fullWidth - gen.width))
              go doc' (c { next = gen.next } : gens) (width + c.fullWidth) false cs
            Nothing -> do
              let doc' = doc <> power Dodo.space c.fullWidth
              go doc' (c { next = StpNil } : gens) (width + c.fullWidth) done cs
    go mempty List.Nil 0 true columns

type DocAnnStk a = List (Either a (Doc a))

newtype DocBoxBuffer a = DocBoxBuffer
  { currentIndent :: Doc a
  , currentLine :: DocAnnStk a
  , currentWidth :: Int
  , lines :: DocBox a
  }

-- | A printer which can lift a Doc into DocBox. It is assumed that
-- | the Doc's annotations respect a distributive law:
-- | ``` purescript
-- | annotate ann (a <> b) = annotate ann a <> annotate ann b
-- | ````
docBox :: forall a. Printer (DocBoxBuffer a) a (DocBox a)
docBox = Printer
  { emptyBuffer
  , writeText
  , writeIndent
  , writeBreak
  , enterAnnotation
  , leaveAnnotation
  , flushBuffer
  }
  where
  emptyBuffer :: DocBoxBuffer a
  emptyBuffer = DocBoxBuffer
    { currentIndent: mempty
    , currentLine: List.Nil
    , currentWidth: 0
    , lines: DocEmpty
    }

  writeText :: Int -> String -> DocBoxBuffer a -> DocBoxBuffer a
  writeText width text (DocBoxBuffer buff) = do
    let
      doc' = Internal.Text width text
      line = case buff.currentLine of
        Right doc : rest ->
          Right (doc <> doc') : rest
        rest ->
          Right doc' : rest
    DocBoxBuffer buff
      { currentLine = line
      , currentWidth = buff.currentWidth + width
      }

  writeIndent :: Int -> String -> DocBoxBuffer a -> DocBoxBuffer a
  writeIndent width text (DocBoxBuffer buff) = do
    let doc = Internal.Text width text
    DocBoxBuffer buff
      { currentIndent = buff.currentIndent <> doc
      , currentWidth = buff.currentWidth + width
      }

  writeBreak :: DocBoxBuffer a -> DocBoxBuffer a
  writeBreak (DocBoxBuffer buff) = do
    let line = stkToDoc buff.currentLine
    DocBoxBuffer buff
      { currentIndent = mempty
      , currentLine = List.filter isLeft buff.currentLine
      , currentWidth = 0
      , lines = buff.lines `vappend` DocLine (buff.currentIndent <> line) buff.currentWidth
      }

  enterAnnotation :: a -> List a -> DocBoxBuffer a -> DocBoxBuffer a
  enterAnnotation ann _ (DocBoxBuffer buff) =
    DocBoxBuffer buff
      { currentLine = Left ann : buff.currentLine
      }

  leaveAnnotation :: a -> List a -> DocBoxBuffer a -> DocBoxBuffer a
  leaveAnnotation _ _ (DocBoxBuffer buff) = do
    let
      line = case buff.currentLine of
        Right doc : Left ann : rest ->
          Right (annotate ann doc) : rest
        Left _ : rest ->
          rest
        _ ->
          unsafeCrashWith "leaveAnnotation: docs and annotations must be interleaved"
    DocBoxBuffer buff
      { currentLine = line
      }

  flushBuffer :: DocBoxBuffer a -> DocBox a
  flushBuffer (DocBoxBuffer buff)
    | isEmpty buff.lines && List.null buff.currentLine =
        DocEmpty
    | otherwise = do
        let line = stkToDoc buff.currentLine
        buff.lines `vappend` DocLine (buff.currentIndent <> line) buff.currentWidth

  stkToDoc :: DocAnnStk a -> Doc a
  stkToDoc = foldl
    ( \doc -> case _ of
        Left ann ->
          annotate ann doc
        Right doc' ->
          doc' <> doc
    )
    mempty
