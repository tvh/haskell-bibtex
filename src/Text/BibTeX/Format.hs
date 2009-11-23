module Text.BibTeX.Format where

import qualified Text.BibTeX.Entry as Entry

import Data.List (intersperse, )

import qualified Data.Char as Char


entry :: Entry.T -> String
entry (Entry.Cons entryType bibId items) =
   let formatItem (name, value) =
         "  "++name++" = {"++value++"},\n"
   in  "@" ++ entryType ++ "{" ++ bibId ++ ",\n" ++
       concatMap formatItem items ++
       "}\n\n"


enumerate :: [String] -> String
enumerate (name:[]) = name
enumerate names =
   let name1:name0:rest = reverse names
   in  foldl (\enum s -> s++", "++enum) (name0++" and "++name1) rest
       --foldl (flip ((++).(++", "))) (name0 ++ " and " ++name1) rest

commaSepList :: [String] -> String
commaSepList = sepList ','

sepList :: Char -> [String] -> String
sepList sep = concat . intersperse (sep:" ")
