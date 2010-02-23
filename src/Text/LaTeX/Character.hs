module Text.LaTeX.Character (
   toUnicodeString,
   fromUnicodeString,
   table,
   ) where

import Data.List.HT (multiReplace, )
import Data.Char (chr, ord, )
import qualified Data.Map as Map


toUnicodeString :: String -> String
toUnicodeString = multiReplace table

fromUnicodeString :: String -> String
fromUnicodeString =
   concatMap (\c -> Map.findWithDefault [c] c fromMap)
--   multiReplace (map swap table)

{-# DEPRECATED table "use toUnicodeString or fromUnicodeString" #-}
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


fromMap :: Map.Map Char String
fromMap =
   Map.fromList
      (do (base, variants) <- accents
          (accent, code) <- variants
          return (chr code, '\\':accent:'{':base:'}':""))
   `Map.union`
   Map.fromList
      (map (\(ident, code) -> (chr code, "\\"++ident++"{}")) specialChars)
   `Map.union`
   Map.fromList
      -- curly braces around dollars assert that no $$ can occur in the output
      (map (\(ident, code) -> (chr code, "{$\\"++ident++"$}")) mathChars)
   `Map.union`
   Map.fromList
      (map (\c -> (c, '\\':c:[])) escapedChars)


accents :: [(Char, [(Char, Int)])]
accents =
   ('A', ('`', 192) : ('\'', 193) : ('^', 194) : ('~', 195) : ('"', 196) : []) :
   ('E', ('`', 200) : ('\'', 201) : ('^', 202) : ('"', 203) : []) :
   ('I', ('`', 204) : ('\'', 205) : ('^', 206) : ('"', 207) : []) :
   ('N', ('~', 209) : []) :
   ('O', ('`', 210) : ('\'', 211) : ('^', 212) : ('~', 213) : ('"', 214) : []) :
   ('U', ('`', 217) : ('\'', 218) : ('^', 219) : ('"', 220) : []) :
   ('Y', ('"', 223) : ('\'', 221) : []) :
   ('C', ('c', 199) : ('\'', 262) : ('^', 264) : ('.', 266) : ('v', 268) : []) :
   ('S', ('c', 350) : ('\'', 346) : ('^', 348) : ('v', 352) : []) :
   ('a', ('`', 224) : ('\'', 225) : ('^', 227) : ('"', 228) : []) :
   ('e', ('`', 232) : ('\'', 233) : ('^', 234) : ('"', 235) : []) :
   ('i', ('`', 236) : ('\'', 237) : ('^', 238) : ('"', 239) : []) :
   ('n', ('~', 241) : []) :
   ('o', ('`', 242) : ('\'', 243) : ('^', 244) : ('~', 245) : ('"', 246) : []) :
   ('u', ('`', 249) : ('\'', 250) : ('^', 251) : ('"', 252) : []) :
   ('y', ('"', 255) : ('\'', 253) : []) :
   ('c', ('c', 231) : ('\'', 263) : ('^', 265) : ('.', 267) : ('v', 269) : []) :
   ('s', ('c', 351) : ('\'', 347) : ('^', 349) : ('"', 223) : ('v', 353) : []) :
   []

specialChars :: [(String, Int)]
specialChars =
   ("cc", 231) :
   ("cC", 199) :
   ("aa", 229) :
   ("AA", 197) :
   ("i",  239) :
   ("l",  321) :
   ("L",  322) :
   ("ss", 223) :
   ("3",  223) :
   ("o",  248) :
   ("O",  216) :
   ("ae", 230) :
   ("AE", 198) :
   ("S", 167) :
   ("pounds", 163) :
   ("euro", 8364) :
   ("copyright", 169) :

   ("textbackslash", ord '\\') :
   ("textasciitilde", ord '~') :
   ("textasciicircum", ord '^') :
   ("textless", ord '<') :
   ("textgreater", ord '>') :
   ("textdollar", ord '$') :
   ("textexclamdown", 161) :
   ("textquestiondown", 191) :
   ("textquotedblleft", ord '"') :
   []

mathChars :: [(String, Int)]
mathChars =
   ("Alpha",      913) :
   ("Beta",       914) :
   ("Gamma",      915) :
   ("Delta",      916) :
   ("Epsilon",    917) :
   ("Zeta",       918) :
   ("Eta",        919) :
   ("Theta",      920) :
   ("Iota",       921) :
   ("Kappa",      922) :
   ("Lambda",     923) :
   ("Mu",         924) :
   ("Nu",         925) :
   ("Xi",         926) :
   ("Omikron",    927) :
   ("Pi",         928) :
   ("Rho",        929) :
   ("Sigma",      931) :
   ("Tau",        932) :
   ("Upsilon",    933) :
   ("Phi",        934) :
   ("Chi",        935) :
   ("Psi",        936) :
   ("Omega",      937) :
   ("alpha",      945) :
   ("beta",       946) :
   ("gamma",      947) :
   ("delta",      948) :
   ("epsilon",    949) :
   ("zeta",       950) :
   ("eta",        951) :
   ("theta",      952) :
   ("iota",       953) :
   ("kappa",      954) :
   ("lambda",     955) :
   ("mu",         956) :
   ("nu",         957) :
   ("xi",         958) :
   ("omikron",    959) :
   ("pi",         960) :
   ("rho",        961) :
   ("sigma",      963) :
   ("tau",        964) :
   ("upsilon",    965) :
   ("phi",        966) :
   ("chi",        967) :
   ("psi",        968) :
   ("omega",      969) :
   []

escapedChars :: [Char]
escapedChars = "$%&_#{}"
