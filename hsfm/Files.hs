module Files where

import Control.Monad
import Data.Char (toLower)
import Data.Maybe
import Data.Time
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Graphics.UI.Gtk
import System.GIO
import System.Glib.GDateTime
import Text.Printf

import qualified Data.ByteString.UTF8 as UTF8

data FMInfo = FMInfo {
    fIcon :: Pixbuf,               -- icon
    fName :: String,               -- file name
    fDesc :: String,               -- mime type description
    fSize :: Integer,              -- file size
    fTime :: UTCTime               -- modified time
}

data Sorting =
    SortByName |
    SortByType |
    SortBySize |
    SortByDate deriving (Eq, Ord, Enum, Show)

dumpFMInfo :: FMInfo -> String
dumpFMInfo (FMInfo _ n d s t) = printf f n d s $ show t where
    f = "FMInfo { fName = \"%s\", fDesc = \"%s\", fSize = %d, fTime = %s }"

compareFMInfos :: FMInfo -> FMInfo -> Sorting -> Ordering
compareFMInfos (FMInfo _ n1 d1 s1 t1) (FMInfo _ n2 d2 s2 t2) sorting
    | d1 == "folder" = if d2 == "folder"
                          then (if sorting == SortByDate
                              then compare t1 t2
                              else compareCI n1 n2)
                          else LT
    | d2 == "folder" = GT
    | otherwise = case sorting of
                       SortByName -> compareCI n1 n2
                       SortByType ->
                            let cmp = compareCI d1 d2
                            in
                            if cmp == EQ
                                then compareCI n1 n2
                                else cmp
                       SortBySize -> compare s1 s2
                       SortByDate -> compare t1 t2

compareCI :: String -> String -> Ordering
compareCI a b = compare (map toLower a) (map toLower b)

directoryGetFMInfos :: FilePath -> IO [FMInfo]
directoryGetFMInfos directory = do
    infos <- directoryGetFileInfos directory
    fInfos <- mapM (\info -> do
        -- Get Icon.
        icon <- fileInfoGetIcon info
        iconTheme <- iconThemeGetDefault
        iconInfo <- iconThemeLookupByGIcon iconTheme icon 24 IconLookupUseBuiltin
        pixbuf <- case iconInfo of
                    Just ii -> iconInfoLoadIcon ii
                    Nothing -> liftM fromJust $ iconThemeLoadIcon iconTheme "unknown" 24 IconLookupUseBuiltin

        let
            -- Get file name.
            name = fromJust $ fileInfoGetName info
            -- File size.
            size = toInteger $ fileInfoGetSize info
            -- File modified time.
            time = gTimeValToUTCTime $ fileInfoGetModificationTime info
            -- File mime description.
            Just contentType = fileInfoGetContentType info
            desc = contentTypeGetDescription contentType

        return $ FMInfo pixbuf (UTF8.toString name) desc size time
        ) infos
    return fInfos

directoryGetFileInfos :: FilePath -> IO [FileInfo]
directoryGetFileInfos directory = do
    let dir = fileFromPath (UTF8.fromString directory)
    enumerator <- fileEnumerateChildren dir "*" [] Nothing
    fileEnumeratorGetFileInfos enumerator

fileEnumeratorGetFileInfos :: FileEnumeratorClass enumerator => enumerator -> IO [FileInfo]
fileEnumeratorGetFileInfos enum = do
    fileInfo <- fileEnumeratorNextFile enum Nothing
    case fileInfo of
         Just info -> do
             infos <- fileEnumeratorGetFileInfos enum
             return $ info : infos
         Nothing -> return []

formatFileSizeForDisplay :: Integer -> String
formatFileSizeForDisplay size
    | size < 2 ^ 10  = humanSize 1 ++ " bytes"
    | size < 2 ^ 20  = humanSize (2 ^ 10)  ++ " KB"
    | size < 2 ^ 30  = humanSize (2 ^ 20)  ++ " MB"
    | size < 2 ^ 40  = humanSize (2 ^ 30)  ++ " GB"
    | size < 2 ^ 50  = humanSize (2 ^ 40)  ++ " TB"
    | size < 2 ^ 60  = humanSize (2 ^ 50)  ++ " PB"
    | size < 2 ^ 70  = humanSize (2 ^ 60)  ++ " EB"
    | size < 2 ^ 80  = humanSize (2 ^ 70)  ++ " ZB"
    | size < 2 ^ 90  = humanSize (2 ^ 80)  ++ " YB"
    | size < 2 ^ 100 = humanSize (2 ^ 90)  ++ " NB"
    | size < 2 ^ 110 = humanSize (2 ^ 100) ++ " DB"
    where humanSize base = printf "%.1f" (integralToDouble size / base) :: String

integralToDouble :: Integral a => a -> Double
integralToDouble v = fromIntegral v :: Double

gTimeValToUTCTime :: GTimeVal -> UTCTime
gTimeValToUTCTime GTimeVal {gTimeValSec  = seconds
                           ,gTimeValUSec = microseconds} =
    posixSecondsToUTCTime . realToFrac . picosecondsToDiffTime $ (toInteger seconds) * 10 ^ 12 + (toInteger microseconds) * 10 ^ 6
