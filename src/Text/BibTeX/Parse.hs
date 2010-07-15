module Text.BibTeX.Parse where

import qualified Text.BibTeX.Entry as Entry

import Text.ParserCombinators.Parsec (Parser, (<|>), )
import qualified Text.ParserCombinators.Parsec as Parsec
import qualified Data.Char as Char

import Control.Monad (liftM, liftM2, liftM3, )
-- import Control.Applicative ((<*), )

import Data.Maybe (catMaybes, )
import Data.List.HT (chop, )
import Data.String.HT (trim, )


{- |
Beware that this and all other parsers do not accept leading spaces,
cf. 'skippingSpace'.
That is when encountering leading white spaces
the parser will just return an empty list.
If you want to parse a file that contains entirely of BibTeX data
you better call @skippingLeadingSpace file@ instead.
However, the @file@ parser is more combinable
and can be used for files that contain both BibTeX and other data
or it can be used for automated filetype checking.
-}
file :: Parser [Entry.T]
file =
   fmap catMaybes $
   Parsec.many
      (skippingSpace
--         ((fmap Just entry <* Parsec.optional (Parsec.char ',')))
         ((do e <- entry; Parsec.optional (Parsec.char ','); return (Just e))
          <|>
          fmap (const Nothing) comment))

{- |
Parse a line that starts with a hash like

> # this is a comment

.
-}
comment :: Parser String
comment =
   do Parsec.char '#'
      fmap trim $ Parsec.manyTill Parsec.anyChar Parsec.newline

{- |
Parse a BibTeX entry like

> @article{author2010title,
>   author = {Firstname Surname},
>   title = {Title},
>   year = 2010,
>   month = jul,
> }

.
-}
entry :: Parser Entry.T
entry =
   do Parsec.char '@'
      entryType <- skippingSpace identifier
      skippingSpace (Parsec.char '{')
      bibId <- skippingSpace (bibIdentifier <|> return "")
      skippingSpace (Parsec.char ',')
      assigns <- assignment `Parsec.sepEndBy` skippingSpace (Parsec.char ',')
      skippingSpace (Parsec.char '}')
      return (Entry.Cons entryType bibId assigns)

{- |
Parse an assignment like

> author = {Firstname Surname}

.
-}
assignment :: Parser (String, String)
assignment =
   do field <- skippingSpace bibIdentifier
      skippingSpace (Parsec.char '=')
      val <- skippingSpace value
      return (field, trim val)

{- |
Parse a value like

> jul

or

> 2010

or

> {Firstname Surname}

or

> "Firstname Surname"

.
-}
value :: Parser String
value =
   Parsec.many1 Parsec.letter <|> -- for fields like: month = jul
   Parsec.many1 Parsec.digit <|>  -- for fields like: year = 2010
   Parsec.between (Parsec.char '{') (Parsec.char '}') (texSequence '}') <|>
   Parsec.between (Parsec.char '"') (Parsec.char '"') (texSequence '"')

{- |
Parse a sequence of 'texBlock's until the occurrence of a closing character.
The closing character is not part of the result.
-}
texSequence :: Char -> Parser String
texSequence closeChar =
   liftM concat (Parsec.many (texBlock closeChar))

{- |
Parse a single character like @a@,
a LaTeX macro call like @\\alpha@
or a block enclosed in curly braces like @{\\\"{a}bc}@.
-}
texBlock :: Char -> Parser String
texBlock closeChar =
   liftM3 (\open body close -> open : body ++ close : [])
      (Parsec.char '{') (texSequence '}') (Parsec.char '}') <|>
   sequence
      [Parsec.char '\\',
       Parsec.oneOf "_{}'`^&%\".,~# " <|> Parsec.letter] <|>
   fmap (:[]) (Parsec.noneOf [closeChar])

{- |
Parse a type of a BibTeX entry like @article@.
-}
identifier :: Parser String
identifier =
   liftM2 (:)
      Parsec.letter
      (Parsec.many Parsec.alphaNum)

{- |
Parse a name of a BibTeX entry like @author2010title@.
-}
bibIdentifier :: Parser String
bibIdentifier =
   Parsec.many1 (Parsec.alphaNum <|> Parsec.oneOf "&;:-_.?+/")

{- |
Extends a parser, such that all trailing spaces are skipped.
It might be more comfortable to skip all leading zeros,
but parser written that way are hard to combine.
This is so, since if you run two parsers in parallel
and both of them expect leading spaces,
then the parser combinator does not know
which one of the parallel parsers to choose.
-}
skippingSpace :: Parser a -> Parser a
skippingSpace p =
   do x <- p
      Parsec.skipMany Parsec.space
      return x

skippingLeadingSpace :: Parser a -> Parser a
skippingLeadingSpace p =
   Parsec.skipMany Parsec.space >> p



-- * Convert contents of BibTeX fields into lists

{- |
Split a string at the commas and remove leading spaces.
-}
splitCommaSepList :: String -> [String]
splitCommaSepList = splitSepList ','

{- |
Split a string containing a list of authors in BibTeX notation.
-}
splitAuthorList :: String -> [String]
splitAuthorList =
   map unwords . chop ("and" ==) . words

splitSepList :: Char -> String -> [String]
splitSepList sep =
   map (dropWhile (' '==)) . chop (sep==)
