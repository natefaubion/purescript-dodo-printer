-- | Common symbols composed out of the ASCII subset of Unicode. For non-ASCII symbols, see `Dodo.Symbols.Unicode`
module Dodo.Symbols.Ascii
  ( module Dodo.Symbols.Ascii
  , module Export
  ) where

import Dodo (Doc, enclose, text)
import Dodo (space) as Export

-- | >>> squotes "·"
-- '·'
squotes :: forall a. Doc a -> Doc a
squotes = enclose squote squote

-- | >>> dquotes "·"
-- "·"
dquotes :: forall a. Doc a -> Doc a
dquotes = enclose dquote dquote

-- | >>> parens "·"
-- (·)
parens :: forall a. Doc a -> Doc a
parens = enclose lparen rparen

-- | >>> angles "·"
-- <·>
angles :: forall a. Doc a -> Doc a
angles = enclose langle rangle

-- | >>> brackets "·"
-- [·]
brackets :: forall a. Doc a -> Doc a
brackets = enclose lbracket rbracket

-- | >>> braces "·"
-- {·}
braces :: forall a. Doc a -> Doc a
braces = enclose lbrace rbrace

-- | >>> squote
-- '
squote :: forall a. Doc a
squote = text "'"

-- | >>> dquote
-- "
dquote :: forall a. Doc a
dquote = text "\""

-- | >>> lparen
-- (
lparen :: forall a. Doc a
lparen = text "("

-- | >>> rparen
-- )
rparen :: forall a. Doc a
rparen = text ")"

-- | >>> langle
-- <
langle :: forall a. Doc a
langle = text "<"

-- | >>> rangle
-- >
rangle :: forall a. Doc a
rangle = text ">"

-- | >>> lbracket
-- [
lbracket :: forall a. Doc a
lbracket = text "["
-- | >>> rbracket
-- ]
rbracket :: forall a. Doc a
rbracket = text "]"

-- | >>> lbrace
-- {
lbrace :: forall a. Doc a
lbrace = text "{"
-- | >>> rbrace
-- }
rbrace :: forall a. Doc a
rbrace = text "}"

-- | >>> semi
-- ;
semi :: forall a. Doc a
semi = text ";"

-- | >>> colon
-- :
colon :: forall a. Doc a
colon = text ":"

-- | >>> comma
-- ,
comma :: forall a. Doc a
comma = text ","

-- | >>> dot
-- .
dot :: forall a. Doc a
dot = text "."

-- | >>> slash
-- /
slash :: forall a. Doc a
slash = text "/"

-- | >>> backslash
-- \\

backslash :: forall a. Doc a
backslash = text "\\"

-- | >>> equals
-- =
equals :: forall a. Doc a
equals = text "="

-- | >>> pipe
-- |
pipe :: forall a. Doc a
pipe = text "|"
