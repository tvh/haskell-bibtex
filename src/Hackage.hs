module Main where

import qualified Text.BibTeX.Format as Format
import qualified Text.BibTeX.Entry as Entry

import qualified Distribution.PackageDescription.Parse as PkgP
import qualified Distribution.PackageDescription as PkgD
import qualified Distribution.Package as Pkg
import qualified Distribution.Verbosity as Verbosity
import Distribution.PackageDescription
                   (PackageDescription, )
import Distribution.Package
                   (PackageIdentifier(..), )
import Distribution.PackageDescription.Parse
                   (parsePackageDescription,
                    readPackageDescription, )
import System.Time (ClockTime(TOD), getClockTime,
                    toCalendarTime, toUTCTime,
                    CalendarTime, ctYear, ctMonth, )

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as TarEnt
import qualified Data.ByteString.Lazy as B
import qualified System.IO as IO

import Distribution.Text (display, )

import Data.List.HT (dropWhileRev, )
import Data.Char    (toLower, isSpace, isAlpha, chr, )
import Data.Version (showVersion, )
import qualified Data.List as List


{- |
See hackage-server:Distribution/Server/Pages/Package.hs
-}
packageURL :: PackageIdentifier -> String
packageURL pkgid = "/package/" ++ display pkgid


fromPackage :: CalendarTime -> PackageDescription -> Entry.T
fromPackage time pkg =
   let author =
          let str = dropWhile isSpace $ PkgD.author pkg
          in  case str of
                 '"' : t -> takeWhile ('"' /=) t
                 _ -> dropWhileRev isSpace $ takeWhile ('<' /=) str
       surname =
          let nameParts = words author
          in  if null nameParts
                then ""
                else filter isAlpha $ last nameParts
       pkgId = PkgD.package pkg
       Pkg.PackageName name = Pkg.pkgName pkgId
       year = ctYear time
       versionStr = showVersion (Pkg.pkgVersion pkgId)
   in  Entry.Cons "Misc"
          (map toLower surname ++ show year ++
           name ++ "-" ++ versionStr) $
       ("author", author) :
       ("title", "{" ++ name ++ ": " ++ PkgD.synopsis pkg ++ "}") :
       ("howpublished",
           "\\url{http://hackage.haskell.org" ++
           packageURL (PkgD.package pkg) ++ "}") :
       ("year", show year) :
       ("month", show (ctMonth time)) :
       ("version", versionStr) :
       ("keywords", "Haskell, " ++ PkgD.category pkg ) :
       ("subtype", "program") :
       []

example :: IO ()
example =
   do now <- toCalendarTime =<< getClockTime
      pkg <- readPackageDescription Verbosity.silent "example.cabal"
      putStrLn (Format.entry $ fromPackage now $ PkgD.packageDescription pkg)


fromTarEntry :: Tar.Entry -> String
fromTarEntry ent =
   if List.isSuffixOf ".cabal" (TarEnt.entryPath ent)
     then
       case TarEnt.entryContent ent of
          TarEnt.NormalFile txt _size ->
             case parsePackageDescription
                     (map (chr . fromIntegral) (B.unpack txt)) of
                PkgP.ParseOk _ pkg ->
                   Format.entry $
                   fromPackage
                      (toUTCTime (TOD (fromIntegral $ TarEnt.entryTime ent) 0))
                      (PkgD.packageDescription pkg)
                PkgP.ParseFailed msg -> show msg
          _ -> ""
     else ""

main :: IO ()
main =
   Tar.foldEntries
      (\entry cont ->
         putStrLn (fromTarEntry entry) >> cont)
      (return ()) (IO.hPutStr IO.stderr) .
   Tar.read =<<
   B.getContents
