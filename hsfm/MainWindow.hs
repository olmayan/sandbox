module MainWindow (mainWindowNew) where

import Files

import Control.Monad
import Control.Monad.Trans (liftIO) -- Maybe delete in the Future
import Data.Char (toLower)
import Data.IORef
import Data.Int
import Data.List (intersect)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events (Event(Key))
import Graphics.UI.Gtk.ModelView.TreeModel
import System.FilePath.Posix
import System.Directory
import System.Glib.UTFString (glibToString)
import System.Time -- should be replaced with Data.Time
import qualified System.Locale as L

data AppData = AppData {
    window       :: Window,
    entryAddr    :: Entry,
    aGoBack      :: Action,
    aGoForward   :: Action,
    rawModel     :: ListStore FMInfo,
    model        :: TypedTreeModelSort FMInfo,
    curPath      :: FilePath,
    backStack    :: [FilePath],
    forwardStack :: [FilePath]
}

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

    -- Build the Go menu.
    aGo <- actionNew "GO" "_Go" Nothing Nothing
    aGoUp <- actionNew "GOUP" "_Up" Nothing $ Just stockGoUp
    aGoBack <- actionNew "GOBACK" "_Back" Nothing $ Just stockGoBack
    aGoForward <- actionNew "GOFORWARD" "_Forward" Nothing $ Just stockGoForward
    aGoHome <- actionNew "GOHOME" "_Home folder" Nothing $ Just stockHome

    -- Set up the global data storage.
    homePath <- getHomeDirectory

    appData <- newIORef AppData {
        window       = window,
        entryAddr    = entryAddr,
        aGoBack      = aGoBack,
        aGoForward   = aGoForward,
        rawModel     = rawModel,
        model        = model,
        curPath      = homePath,
        backStack    = [],
        forwardStack = []
        }

    -- Bind events to actions.
    aGoUp `onActionActivate` (goUp appData)
    aGoBack `set` [ actionSensitive := False ]
    aGoForward `set` [ actionSensitive := False ]
    aGoHome `onActionActivate` (goHome appData)

    -- Create the action group.
    agr <- actionGroupNew "AGR"

    actionGroupAddAction agr aGo
    actionGroupAddActionWithAccel agr aGoUp      (Just "BackSpace")
    actionGroupAddActionWithAccel agr aGoBack    (Just "<Ctrl>Left")
    actionGroupAddActionWithAccel agr aGoForward (Just "<Ctrl>Right")
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
        when (null (mods `intersect` [Control, Alt, Shift]) && glibToString keyName == "Return") $ do
            newPath <- entryAddr `get` entryText
            when (newPath /= "") $ walkPath newPath appData
        return False
    toolbarInsert (castToToolbar toolbar) toolItem (-1)

    -- Initialize tree view.
    tv <- treeViewNewWithModel model
    scrolledWindow `containerAdd` tv
    boxPackStart vbox scrolledWindow PackGrow 0
    onKeyPress tv $ \(Key _ _ _ mods _ _ _ _ keyName _) -> do
        --putStrLn $ glibToString keyName
        when (null (mods `intersect` [Control, Alt, Shift]) && glibToString keyName == "BackSpace") $ goUp appData
        return False

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
    tv `onRowActivated` (\path col -> MainWindow.rowActivated path col appData)
    
    -- TreeView search.
    tv `set` [ treeViewEnableSearch := True
             , treeViewSearchColumn := (makeColumnIdString 2 :: ColumnId String String) ]
    let equalFunc :: String -> TreeIter -> IO Bool
        equalFunc s iter = do
            cIter <- treeModelSortConvertIterToChildIter model iter
            FMInfo { fName = name } <- treeModelGetRow rawModel cIter
            return $ take (length s) name == s
    tv `treeViewSetSearchEqualFunc` (Just equalFunc)

    -- Walking to the default path.
    walkPath homePath appData

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

goUp :: IORef AppData -> IO ()
goUp appData = do
    AppData { curPath = path } <- readIORef appData
    when (not $ isDrive path) $ do
        walkPath (takeDirectory $ dropTrailingPathSeparator path) appData

goHome :: IORef AppData -> IO ()
goHome appData = do
    homePath <- getHomeDirectory
    walkPath homePath appData

rowActivated :: TreePath
             -> TreeViewColumn
             -> IORef AppData
             -> IO ()
rowActivated path col appData = do
    ad@AppData {
        curPath   = curPath,
        model     = model,
        rawModel  = rawModel,
        entryAddr = entryAddr
        } <- readIORef appData

    (Just iter) <- treeModelGetIter model path
    cIter <- treeModelSortConvertIterToChildIter model iter
    fInfo@FMInfo { fName = name
                 , fDesc = desc } <- treeModelGetRow rawModel cIter
    when (desc == "folder") $ do
        let newPath = curPath `combine` name
        walkPath newPath appData
    putStrLn $ dumpFMInfo fInfo
    
walkPath :: FilePath -> IORef AppData -> IO ()
walkPath newPath appData = do
    ad@AppData {
        curPath      = curPath,
        rawModel     = rawModel,
        aGoBack      = aGoBack,
        aGoForward   = aGoForward,
        entryAddr    = entryAddr,
        backStack    = [],
        forwardStack = []
        } <- readIORef appData
    
    let newPath' = addTrailingPathSeparator (if isRelative newPath'' then curPath `combine` newPath'' else newPath'') where 
        newPath'' = normalise newPath
    exists <- doesDirectoryExist newPath'
    readable <- if exists then do
        permissions <- getPermissions newPath'
        return $ readable permissions
                          else return False
    if exists && readable then do
        fInfos <- directoryGetFMInfos newPath'
        listStoreClear rawModel
        mapM_ (rawModel `listStoreAppend`) fInfos
        entryAddr `set` [ entryText := newPath' ]
        let pos = length newPath'
        editableSelectRegion entryAddr pos pos
        
        writeIORef appData ad { curPath = newPath' }        
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
