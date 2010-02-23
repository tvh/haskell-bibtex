module Text.LaTeX.Character where

import Data.List.HT (multiReplace, )
import Data.Tuple.HT (swap, )


toUnicodeString :: String -> String
toUnicodeString = multiReplace table

fromUnicodeString :: String -> String
fromUnicodeString =
   multiReplace (map swap table)


table :: [(String, String)]
table =
   ("\\&",    "&") :
   ("\\~{}",  "~") :
   ("\\\"a",  "ä") :
   ("\\\"o",  "ö") :
   ("\\\"u",  "ü") :
   ("\\\"A",  "Ä") :
   ("\\\"O",  "Ö") :
   ("\\\"U",  "Ü") :
   ("\\ss{}", "ß") :
   ("\\`e",   "è") :
   ("\\'e",   "é") :
   ("\\'a",   "á") :
   ("\\'{\\i}", "í") :
   ("\\'u",   "ú") :
   ("\\'U",   "Ú") :
   ("\\o{}", "ø") :
   ("\\O{}", "Ø") :
   ("\\oe{}", "œ") :
   ("\\OE{}", "Œ") :
   ("\\ae{}", "æ") :
   ("\\AE{}", "Æ") :
   ("\\l{}", "ł") :
   ("\\L{}", "Ł") :
   ("\\c{c}", "ç") :
   ("\\c{C}", "Ç") :
   ("\\~a", "ã") :
   ("\\~A", "Ã") :
   []
