-- | File Manager demo.
--  Author      :  Andy Stewart
--  Copyright   :  (c) 2010 Andy Stewart <lazycat.manatee@gmail.com>

-- | This simple file-manager base on gio.
--
module Main where

import Control.Monad
import Control.Monad.Trans (liftIO) -- Maybe delete in the Future
import Data.Maybe
import Data.IORef
import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.IconTheme
import Graphics.UI.Gtk.ModelView
import System.GIO
import System.Glib.GDateTime
import System.Glib.GError
import qualified System.Locale as L
import System.Time -- should be replaced with Data.Time
import Text.Printf

import qualified Data.ByteString.UTF8 as UTF8

data FMInfo = FMInfo {
    fIcon :: Pixbuf,               -- icon
    fName :: String,               -- file name
    fDesc :: String,               -- mime type description
    fSize :: Integer,              -- file size
    fTime :: ClockTime             -- modified time
}

data SortMode = 
    SortByName |
    SortByType |
    SortBySize |
    SortByDate 

icon :: FMInfo -> Pixbuf
icon FMInfo { fIcon = i } = i

name :: FMInfo -> String
name FMInfo { fName = n } = n

desc :: FMInfo -> String
desc FMInfo { fDesc = d } = d

size :: FMInfo -> Integer
size FMInfo { fSize = s } = s

time :: FMInfo -> ClockTime
time FMInfo { fTime = t } = t

compareFMInfos :: FMInfo -> FMInfo -> Ordering
compareFMInfos a b
    | (desc a) == "folder" = if (desc b) == "folder" then compare (name a) (name b) else LT
    | (desc b) == "folder" = GT
    | otherwise = compare (name a) (name b)

-- | Main.
main :: IO ()
main = do
    -- Init.
    initGUI

    -- Create window.
    window <- windowNew
    windowSetDefaultSize window 900 600
    windowSetPosition window WinPosCenter
    scrolledWindow <- scrolledWindowNew Nothing Nothing
    window `containerAdd` scrolledWindow

    -- Get file infos under specific directory.
    curPath <- newIORef "/"
    path <- readIORef curPath
    fInfos <- directoryGetFMInfos path

    -- Initialize model for the tree view.
    rawModel <- listStoreNew fInfos
    model <- treeModelSortNewWithModel rawModel
    
    -- Set sort functions for the model.
    let nameFunc = \iter1 iter2 -> do
        a <- treeModelGetRow rawModel iter1
        b <- treeModelGetRow rawModel iter2
        return $ compareFMInfos a b
        
    treeSortableSetDefaultSortFunc model $ Just nameFunc
    treeSortableSetSortFunc model 2 nameFunc 

    -- Initialize tree view.
    tv <- treeViewNewWithModel model
    --tv `set` [ treeViewHeadersClickable := True
    --         , treeViewReorderable := True ]
    scrolledWindow `containerAdd` tv

    -- List Icons.
    tvc <- treeViewColumnNew
    set tvc [ treeViewColumnTitle := "Icon"
            , treeViewColumnResizable := True ]
    tv `treeViewAppendColumn` tvc

    rend <- cellRendererPixbufNew
    cellLayoutPackStart tvc rend True
    treeViewColumnPackStart tvc rend True
    cellLayoutSetAttributeFunc tvc rend model $ \iter -> do
        cIter <- treeModelSortConvertIterToChildIter model iter
        fInfo <- treeModelGetRow rawModel cIter
        rend `set` [ cellPixbuf := (icon fInfo) ]

    -- List Name.
    tvc <- treeViewColumnNew
    set tvc [ treeViewColumnTitle := "Name"
            , treeViewColumnResizable := True ]
            --, treeViewColumnClickable := True
            --, treeViewColumnSortIndicator := True
            --, treeViewColumnReorderable := True
            --, treeViewColumnSortOrder := SortAscending ]
    tv `treeViewAppendColumn` tvc

    rend <- cellRendererTextNew
    treeViewColumnPackStart tvc rend True
    cellLayoutSetAttributeFunc tvc rend model $ \iter -> do
        cIter <- treeModelSortConvertIterToChildIter model iter
        fInfo <- treeModelGetRow rawModel cIter
        rend `set` [ cellText := (name fInfo) ]
    tvc `treeViewColumnSetSortColumnId` 2

    -- List file mime description.
    tvc <- treeViewColumnNew
    set tvc [ treeViewColumnTitle := "Description"
            , treeViewColumnResizable := True ]
    tv `treeViewAppendColumn` tvc

    rend <- cellRendererTextNew
    treeViewColumnPackStart tvc rend True
    cellLayoutSetAttributeFunc tvc rend model $ \iter -> do
        cIter <- treeModelSortConvertIterToChildIter model iter
        fInfo <- treeModelGetRow rawModel cIter
        rend `set` [ cellText := (desc fInfo) ]

    -- List file size.
    tvc <- treeViewColumnNew
    set tvc [ treeViewColumnTitle := "Size"
            , treeViewColumnResizable := True ]
    tv `treeViewAppendColumn` tvc

    rend <- cellRendererTextNew
    rend `set` [ cellXAlign := 1.0 ]
    treeViewColumnPackStart tvc rend True
    cellLayoutSetAttributeFunc tvc rend model $ \iter -> do
        cIter <- treeModelSortConvertIterToChildIter model iter
        fInfo <- treeModelGetRow rawModel cIter
        rend `set` [ cellText := (formatFileSizeForDisplay $ size fInfo) ]

    -- List modified time.
    tvc <- treeViewColumnNew
    set tvc [ treeViewColumnTitle := "Modified"
            , treeViewColumnResizable := True ]
    tv `treeViewAppendColumn` tvc

    rend <- cellRendererTextNew
    treeViewColumnPackStart tvc rend True
    cellLayoutSetAttributeFunc tvc rend model $ \iter -> do
        cIter <- treeModelSortConvertIterToChildIter model iter
        fInfo <- treeModelGetRow rawModel cIter
        calTime <- toCalendarTime $ time fInfo
        rend `set` [ cellText := (formatCalendarTime L.defaultTimeLocale "%Y/%m/%d %T" calTime) ]

    -- TreeView Item DoubleClick.
    tv `onRowActivated` (\path col -> do
        putStrLn $ show path
        )

    -- Show window.
    window `onDestroy` mainQuit
    widgetShowAll window

    mainGUI

rowActivated :: TreeViewClass self => self -> TreePath -> TreeViewColumn -> IO()
rowActivated self path col = do
    putStrLn $ show path

printColName col = do
    (Just colname) <- col `get` treeViewColumnTitle
    putStrLn colname

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
            time = gTimeValToClockTime $ fileInfoGetModificationTime info
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

gTimeValToClockTime :: GTimeVal -> ClockTime
gTimeValToClockTime GTimeVal {gTimeValSec  = seconds
                             ,gTimeValUSec = microseconds} =
    TOD (toInteger seconds) (toInteger microseconds * 1000)
