-- | A collection of predefined Unicode values outside of ASCII range. For ASCII, see `Dodo.Symbols.Ascii`.
module Dodo.Symbols.Unicode where

import Dodo (Doc, enclose, text)

-- | Double „99-66“ quotes, as used in German typography.
--
-- >>> d9966quotes "·"
-- „·“
d9966quotes :: forall a. Doc a -> Doc a
d9966quotes = enclose b99dquote t66dquote

-- | Double “66-99” quotes, as used in English typography.
--
-- >>> d6699quotes "·"
-- “·”
d6699quotes :: forall a. Doc a -> Doc a
d6699quotes = enclose t66dquote t99dquote

-- | Single ‚9-6‘ quotes, as used in German typography.
--
-- >>> s96quotes "·"
-- ‚·‘
s96quotes :: forall a. Doc a -> Doc a
s96quotes = enclose b9quote t6quote

-- | Single ‘6-9’ quotes, as used in English typography.
--
-- >>> s69quotes "·"
-- ‘·’
s69quotes :: forall a. Doc a -> Doc a
s69quotes = enclose t6quote t9quote

-- | Double «guillemets», pointing outwards (without adding any spacing).
--
-- >>> dGuillemetsOut "·"
-- «·»
dGuillemetsOut :: forall a. Doc a -> Doc a
dGuillemetsOut = enclose ldGuillemet rdGuillemet

-- | Double »guillemets«, pointing inwards (without adding any spacing).
--
-- >>> dGuillemetsIn "·"
-- »·«
dGuillemetsIn :: forall a. Doc a -> Doc a
dGuillemetsIn = enclose rdGuillemet ldGuillemet

-- | Single ‹guillemets›, pointing outwards (without adding any spacing).
--
-- >>> sGuillemetsOut "·"
-- ‹·›
sGuillemetsOut :: forall a. Doc a -> Doc a
sGuillemetsOut = enclose lsGuillemet rsGuillemet

-- | Single ›guillemets‹, pointing inwards (without adding any spacing).
--
-- >>> sGuillemetsIn "·"
-- ›·‹
sGuillemetsIn :: forall a. Doc a -> Doc a
sGuillemetsIn = enclose rsGuillemet lsGuillemet

-- | Bottom „99“ style double quotes.
--
-- >>> b99dquote
-- „
b99dquote :: forall a. Doc a
b99dquote = text "„"

-- | Top “66” style double quotes.
--
-- >>> t66dquote
-- “
t66dquote :: forall a. Doc a
t66dquote = text "“"

-- | Top “99” style double quotes.
--
-- >>> t99dquote
-- ”
t99dquote :: forall a. Doc a
t99dquote = text "”"

-- | Bottom ‚9‘ style single quote.
--
-- >>> b9quote
-- ‚
b9quote :: forall a. Doc a
b9quote = text "‚"

-- | Top ‘66’ style single quote.
--
-- >>> t6quote
-- ‘
t6quote :: forall a. Doc a
t6quote = text "‘"

-- | Top ‘9’ style single quote.
--
-- >>> t9quote
-- ’
t9quote :: forall a. Doc a
t9quote = text "’"

-- | Right-pointing double guillemets
--
-- >>> rdGuillemet
-- »
rdGuillemet :: forall a. Doc a
rdGuillemet = text "»"

-- | Left-pointing double guillemets
--
-- >>> ldGuillemet
-- «
ldGuillemet :: forall a. Doc a
ldGuillemet = text "«"

-- | Right-pointing single guillemets
--
-- >>> rsGuillemet
-- ›
rsGuillemet :: forall a. Doc a
rsGuillemet = text "›"

-- | Left-pointing single guillemets
--
-- >>> lsGuillemet
-- ‹
lsGuillemet :: forall a. Doc a
lsGuillemet = text "‹"

-- | >>> bullet
-- •
bullet :: forall a. Doc a
bullet = text "•"

-- | >>> endash
-- –
endash :: forall a. Doc a
endash = text "–"

-- | >>> euro
-- €
euro :: forall a. Doc a
euro = text "€"

-- | >>> cent
-- ¢
cent :: forall a. Doc a
cent = text "¢"

-- | >>> yen
-- ¥
yen :: forall a. Doc a
yen = text "¥"

-- | >>> pound
-- £
pound :: forall a. Doc a
pound = text "£"
