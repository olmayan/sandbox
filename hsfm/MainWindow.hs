module MainWindow (mainWindowNew) where

import Files

import Control.Monad
import Control.Monad.Trans (liftIO) -- Maybe delete in the Future
import Data.IORef
import Data.List (intersect)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events (Event(Key))
--import Graphics.UI.Gtk.General.IconTheme
--import Graphics.UI.Gtk.ModelView
import System.FilePath.Posix
import System.Directory
--import System.Glib.GDateTime
--import System.Glib.GError
import System.Glib.UTFString
import System.Time -- should be replaced with Data.Time
import qualified System.Locale as L

-- | Main.
mainWindowNew :: IO Window
mainWindowNew = do
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
    aGoHome `onActionActivate` (goHome curPath rawModel entryAddr)

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
        --putStrLn $ glibToString keyName
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
        FMInfo { fIcon = icon } <- treeModelGetRow rawModel cIter
        rend `set` [ cellPixbuf := icon ]

    -- List Name.
    tvc <- treeViewColumnNew
    set tvc [ treeViewColumnTitle := "Name"
            , treeViewColumnResizable := True ]
    tv `treeViewAppendColumn` tvc

    rend <- cellRendererTextNew
    treeViewColumnPackStart tvc rend True
    cellLayoutSetAttributeFunc tvc rend model $ \iter -> do
        cIter <- treeModelSortConvertIterToChildIter model iter
        FMInfo { fName = name } <- treeModelGetRow rawModel cIter
        rend `set` [ cellText := name ]
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
        FMInfo { fDesc = desc } <- treeModelGetRow rawModel cIter
        rend `set` [ cellText := desc ]
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
        FMInfo { fSize = size } <- treeModelGetRow rawModel cIter
        rend `set` [ cellText := formatFileSizeForDisplay size ]
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
        FMInfo { fTime = time } <- treeModelGetRow rawModel cIter
        calTime <- toCalendarTime time
        rend `set` [ cellText := (formatCalendarTime L.defaultTimeLocale "%Y/%m/%d %T" calTime) ]
    tvc `treeViewColumnSetSortColumnId` 5

    -- TreeView Item DoubleClick.
    tv `onRowActivated` (\path col -> MainWindow.rowActivated path col curPath model rawModel entryAddr)

    -- Walking to the default path.
    homePath <- getHomeDirectory
    walkPath homePath curPath rawModel entryAddr

    -- On destroy quit program.
    window `onDestroy` mainQuit

    return window

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

goHome :: IORef FilePath -> ListStore FMInfo -> Entry -> IO ()
goHome curPath rawModel entryAddr = do
    homePath <- getHomeDirectory
    walkPath homePath curPath rawModel entryAddr

rowActivated :: TreePath
             -> TreeViewColumn
             -> IORef String
             -> TypedTreeModelSort FMInfo
             -> ListStore FMInfo
             -> Entry
             -> IO ()
rowActivated path col curPath model rawModel entryAddr = do
    (Just iter) <- treeModelGetIter model path
    cIter <- treeModelSortConvertIterToChildIter model iter
    fInfo@FMInfo { fName = name
                 , fDesc = desc } <- treeModelGetRow rawModel cIter
    when (desc == "folder") $ do
        path <- readIORef curPath
        let newPath = path `combine` name
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
