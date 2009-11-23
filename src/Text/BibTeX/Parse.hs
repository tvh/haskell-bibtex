module Text.BibTeX.Parse where

import qualified Text.BibTeX.Entry as Entry

import Text.ParserCombinators.Parsec (Parser, (<|>), )
import qualified Text.ParserCombinators.Parsec as Parsec
import qualified Data.Char as Char

import Control.Monad (liftM, liftM2, liftM3, )

import Data.List.HT (chop, )
import Data.String.HT (trim, )


entry :: Parser Entry.T
entry =
   do Parsec.char '@'
      entryType <- skippingSpace identifier
      skippingSpace (Parsec.char '{')
      bibId <- skippingSpace (identifier <|> return "")
      skippingSpace (Parsec.char ',')
      assigns <- assignment `Parsec.sepEndBy` skippingSpace (Parsec.char ',')
      skippingSpace (Parsec.char '}')
      return (Entry.Cons entryType bibId assigns)


assignment :: Parser (String, String)
assignment =
   do field <- skippingSpace identifier
      skippingSpace (Parsec.char '=')
      skippingSpace (Parsec.char '{')
      value <- texSequence
      skippingSpace (Parsec.char '}')
      return (field, trim value)

texSequence :: Parser String
texSequence =
   liftM concat (Parsec.many texBlock)

texBlock :: Parser String
texBlock =
{-
   (liftM (("{"++) . (++"}"))
      (Parsec.between (Parsec.char '{') (Parsec.char '}')
                      texSequence)) <|>
-}
   liftM3 (\open body close -> open : body ++ close : [])
      (Parsec.char '{') texSequence (Parsec.char '}') <|>
   sequence
      [Parsec.char '\\',
       Parsec.oneOf "{}'&\"" <|> Parsec.letter] <|>
   Parsec.count 1 (Parsec.noneOf ['}'])

identifier :: Parser String
identifier =
   liftM2 (:)
      Parsec.letter
      (Parsec.many Parsec.alphaNum)

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
