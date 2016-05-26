-- | File Manager demo.
--  Author      :  Andy Stewart
--  Copyright   :  (c) 2010 Andy Stewart <lazycat.manatee@gmail.com>

-- | This simple file-manager base on gio.
--
module Main where

import Control.Monad
import Control.Monad.Trans (liftIO) -- Maybe delete in the Future
--import Data.CaseInsensitive (CI)
import Data.Char (toLower)
import Data.Maybe
import Data.IORef
import Data.List (intersect)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events (Event(Key))
import Graphics.UI.Gtk.General.IconTheme
import Graphics.UI.Gtk.ModelView
import System.FilePath.Posix
import System.Directory
import System.GIO
import System.Glib.GDateTime
import System.Glib.GError
import System.Glib.UTFString
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

data Sorting =
    SortByName |
    SortByType |
    SortBySize |
    SortByDate deriving (Eq, Ord, Enum, Show)

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

dumpFMInfo :: FMInfo -> String
dumpFMInfo FMInfo { fName = n
                  , fDesc = d
                  , fSize = s
                  , fTime = t } = printf f n d s $ show t where
    f = "FMInfo { fName = \"%s\", fDesc = \"%s\", fSize = %d, fTime = %s }"

compareFMInfos :: FMInfo -> FMInfo -> Sorting -> Ordering
compareFMInfos a b sorting
    | (desc a) == "folder" = if (desc b) == "folder"
                                then (if sorting == SortByDate
                                    then compare (time a) (time b)
                                    else compareCI (name a) (name b))
                                else LT
    | (desc b) == "folder" = GT
    | otherwise = case sorting of
                       SortByName -> compare (name a) (name b)
                       SortByType ->
                            let cmp = compareCI (desc a) (desc b)
                            in
                            if cmp == EQ
                                then compareCI (name a) (name b)
                                else cmp
                       SortBySize -> compare (size a) (size b)
                       SortByDate -> compare (time a) (time b)

compareCI :: String -> String -> Ordering
compareCI a b = compare (map toLower a) (map toLower b)

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
    entryAddr <- entryNew
    vbox <- vBoxNew False 0
    hbox <- hBoxNew False 0
    window `containerAdd` vbox

    -- Get file infos under specific directory.
    curPath <- newIORef (""::FilePath)

    -- Initialize model for the tree view.
    rawModel <- listStoreNew ([]::[FMInfo])
    model <- treeModelSortNewWithModel rawModel

    -- Set sort functions for the model.
    let sortFunc = \iter1 iter2 sorting -> do
        a <- treeModelGetRow rawModel iter1
        b <- treeModelGetRow rawModel iter2
        return $ compareFMInfos a b sorting

    --treeSortableSetDefaultSortFunc model $ Just $ \iter1 iter2 -> sortFunc iter1 iter2 SortByName
    treeSortableSetSortFunc model 2 $ \iter1 iter2 -> sortFunc iter1 iter2 SortByName
    treeSortableSetSortFunc model 3 $ \iter1 iter2 -> sortFunc iter1 iter2 SortByType
    treeSortableSetSortFunc model 4 $ \iter1 iter2 -> sortFunc iter1 iter2 SortBySize
    treeSortableSetSortFunc model 5 $ \iter1 iter2 -> sortFunc iter1 iter2 SortByDate

    -- Set initial sorting.
    treeSortableSetSortColumnId model 2 SortAscending

    -- Create the action group.
    agr <- actionGroupNew "AGR"
    
    -- Building the Go menu.
    aGo <- actionNew "GO" "_Go" Nothing Nothing
    
    aGoUp <- actionNew "GOUP" "_Up" Nothing $ Just stockGoUp
    aGoUp `onActionActivate` (goUp curPath rawModel entryAddr)
    
    aGoBack <- actionNew "GOBACK" "_Back" Nothing $ Just stockGoBack

    aGoForward <- actionNew "GOFORWARD" "_Forward" Nothing $ Just stockGoForward

    aGoHome <- actionNew "GOHOME" "_Home folder" Nothing $ Just stockHome

    actionGroupAddAction agr aGo
    actionGroupAddAction agr aGoUp
    actionGroupAddAction agr aGoBack
    actionGroupAddAction agr aGoForward
    actionGroupAddAction agr aGoHome

    -- Initialize the menu bar.
    ui <- uiManagerNew
    ui `uiManagerAddUiFromString` uiDecl
    uiManagerInsertActionGroup ui agr 0
    
    -- Take the menu from the UI manager.
    (Just menubar) <- ui `uiManagerGetWidget` "/ui/menubar"
    boxPackStart vbox menubar PackNatural 0
    
    -- Take the toolbar from the UI manager.
    (Just toolbar) <- ui `uiManagerGetWidget` "/ui/toolbar"
    boxPackStart vbox toolbar PackNatural 0
    
    -- Creating the address string
    toolItem <- toolItemNew
    toolItem `set` [ toolItemExpand := True ]
    toolItem `containerAdd` entryAddr
    onKeyPress entryAddr $ \(Key _ _ _ mods _ _ _ _ keyName _) -> do
        when (mods `intersect` [Control, Alt, Shift] == [] && glibToString keyName == "Return") $ do
            newPath <- entryAddr `get` entryText
            when (newPath /= "") $ walkPath newPath curPath rawModel entryAddr
        return False
    toolbarInsert (castToToolbar toolbar) toolItem (-1)

    -- Initialize tree view.
    tv <- treeViewNewWithModel model
    tv `set` [ treeViewEnableSearch := True]
    scrolledWindow `containerAdd` tv
    boxPackStart vbox scrolledWindow PackGrow 0
    onKeyPress tv $ \(Key _ _ _ mods _ _ _ _ keyName _) -> do
        putStrLn $ glibToString keyName
        when (elem mods [[], [Alt2]] && glibToString keyName == "BackSpace") $ goUp curPath rawModel entryAddr
        return True

    -- List Icons.
    tvc <- treeViewColumnNew
    tvc `set` [ treeViewColumnTitle := "Icon"
              , treeViewColumnResizable := True ]
    tv `treeViewAppendColumn` tvc

    rend <- cellRendererPixbufNew
    treeViewColumnPackStart tvc rend True
    cellLayoutSetAttributeFunc tvc rend model $ \iter -> do
        cIter <- treeModelSortConvertIterToChildIter model iter
        fInfo <- treeModelGetRow rawModel cIter
        rend `set` [ cellPixbuf := (icon fInfo) ]

    -- List Name.
    tvc <- treeViewColumnNew
    set tvc [ treeViewColumnTitle := "Name"
            , treeViewColumnResizable := True ]
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
    tvc `treeViewColumnSetSortColumnId` 3

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
    tvc `treeViewColumnSetSortColumnId` 4

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
    tvc `treeViewColumnSetSortColumnId` 5

    -- TreeView Item DoubleClick.
    tv `onRowActivated` (\path col -> Main.rowActivated path col curPath model rawModel entryAddr)

    -- Walking to the default path.
    appPath <- getCurrentDirectory
    walkPath appPath curPath rawModel entryAddr

    -- Show window.
    window `onDestroy` mainQuit
    widgetShowAll window

    mainGUI

uiDecl :: String
uiDecl = "<ui>\
\           <menubar>\
\             <menu action=\"GO\">\
\               <menuitem action=\"GOUP\" />\
\               <menuitem action=\"GOBACK\" />\
\               <menuitem action=\"GOFORWARD\" />\
\               <menuitem action=\"GOHOME\" />\
\             </menu>\
\           </menubar>\
\           <toolbar>\
\             <toolitem action=\"GOUP\" />\
\             <toolitem action=\"GOBACK\" />\
\             <toolitem action=\"GOFORWARD\" />\
\             <toolitem action=\"GOHOME\" />\
\           </toolbar>\
\         </ui>"

goUp :: IORef FilePath -> ListStore FMInfo -> Entry -> IO ()
goUp curPath rawModel entryAddr = do
    path <- readIORef curPath
    when (not $ isDrive path) $ do
        walkPath (takeDirectory $ dropTrailingPathSeparator path) curPath rawModel entryAddr

rowActivated :: TreePath
             -> TreeViewColumn
             -> IORef String
             -> TypedTreeModelSort FMInfo
             -> ListStore FMInfo
             -> Entry
             -> IO()
rowActivated path col curPath model rawModel entryAddr = do
    (Just iter) <- treeModelGetIter model path
    cIter <- treeModelSortConvertIterToChildIter model iter
    fInfo <- treeModelGetRow rawModel cIter
    when (desc fInfo == "folder") $ do
        path <- readIORef curPath
        let newPath = (path) `combine` (name fInfo)
        putStrLn newPath
        walkPath newPath curPath rawModel entryAddr
    putStrLn $ dumpFMInfo fInfo
    
walkPath :: FilePath -> IORef FilePath -> ListStore FMInfo -> Entry -> IO ()
walkPath newPath curPath rawModel entryAddr = do
    cPath <- readIORef curPath
    let newPath' = addTrailingPathSeparator (if isRelative newPath'' then cPath `combine` newPath'' else newPath'') where 
        newPath'' = normalise newPath
    exists <- doesDirectoryExist newPath'
    readable <- if exists then do
        permissions <- getPermissions newPath'
        return $ readable permissions
                          else return False
    if exists && readable then do
        writeIORef curPath newPath'
        fInfos <- directoryGetFMInfos newPath'
        listStoreClear rawModel
        mapM_ (rawModel `listStoreAppend`) fInfos
        entryAddr `set` [ entryText := newPath' ]
        let pos = length newPath'
        editableSelectRegion entryAddr pos pos
    else do
        parent <- widgetGetToplevel entryAddr
        dialog <- messageDialogNew
            (Just $ castToWindow parent)
            [DialogModal, DialogDestroyWithParent]
            MessageError
            ButtonsOk
            (if exists then "No permission to read the directory." else "Directory does not exist.")

        dialogRun dialog
        widgetDestroy dialog

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
