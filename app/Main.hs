module Main where

import Control.Monad (join, void, unless, when)
import Control.Monad.Trans (liftIO)
import Data.List (sortBy, findIndex)
import Data.Maybe (fromMaybe)
import System.Directory (copyFileWithMetadata, doesDirectoryExist)
import System.Exit (die)
import System.FilePath.Posix (combine, takeDirectory, takeBaseName)

import qualified Brick.AttrMap as BA
import qualified Brick.Main as BM
import qualified Brick.Types as BT
import qualified Brick.Util as BU
import qualified Brick.Widgets.Border as BWB
import qualified Brick.Widgets.Border.Style as BWBS
import qualified Brick.Widgets.Core as BWC
import qualified Brick.Widgets.Edit as BWE
import qualified Brick.Widgets.List as BWL
import qualified Control.Logging as L
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.Algorithm.Diff as Diff
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as Set
import qualified Data.IORef as IORef
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Time as Time
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Attributes as VA
import qualified Options.Applicative as O
import qualified System.Hclip as Hclip
import qualified System.IO.Memoize as Memoize
import qualified Text.Fuzzy as TF
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import qualified Text.Tabl as Table

import qualified Cipher
import qualified CommandParsers as C
import qualified DB
import qualified IOUtils
import qualified TextTransform
import qualified BabashkaPod as BPod

import Types

appVersion :: String
appVersion = "0.1.0"

type SelectedIndex = Int
type ResourceName = String
data DirectionKey = UP | DOWN | LEFT | RIGHT deriving (Show)

data GenericEditOptions = GERenameKey | GEAddKey | GEAddChildKey | GEDeleteKey deriving (Eq, Show)
data UIMode = BROWSE | BROWSE_EMPTY | BROWSE_HELP | GENERIC_EDIT GenericEditOptions | SORT deriving (Eq, Show)
data ValidationResult = VRSuccess | VRIgnore | VRFailed T.Text

data AppState = AppState {
  _plainNodes :: [PlainNode],
  _selectedChildKeys :: [PlainKey],
  _selectPath :: [(ParentId, SelectedIndex)],
  _currentPath :: [PlainKey],
  _ekey :: EncryptionKey,
  _keysList :: BWL.GenericList ResourceName Vec.Vector PlainKey,
  _uiMode :: UIMode,
  _sortPatternEditor :: BWE.Editor T.Text ResourceName,
  _genericEditor :: BWE.Editor T.Text ResourceName,
  _genericEditPrompt :: T.Text
} deriving (Show)

buildBrickApp :: IO (BM.App AppState e ResourceName)
buildBrickApp = do
  let
    toColor "black"         = VA.black
    toColor "red"           = VA.red
    toColor "green"         = VA.green
    toColor "yellow"        = VA.yellow
    toColor "blue"          = VA.blue
    toColor "magenta"       = VA.magenta
    toColor "cyan"          = VA.cyan
    toColor "white"         = VA.white
    toColor "brightBlack"   = VA.brightBlack
    toColor "brightRed"     = VA.brightRed
    toColor "brightGreen"   = VA.brightGreen
    toColor "brightYellow"  = VA.brightYellow
    toColor "brightBlue"    = VA.brightBlue
    toColor "brightMagenta" = VA.brightMagenta
    toColor "brightCyan"    = VA.brightCyan
    toColor "brightWhite"   = VA.brightWhite
    toColor _               = VA.cyan

  selectedColor <- toColor <$> IOUtils.getEnvWithDefault "STASH_TUI_COLOR_SELECTED" "cyan"
  currentPathColor <- toColor <$> IOUtils.getEnvWithDefault "STASH_TUI_COLOR_CURRENT_PATH" "white"
  sortPatternTextColor <- toColor
    <$> IOUtils.getEnvWithDefault "STASH_TUI_COLOR_SORT_PATTERN" "white"

  return BM.App
    { appDraw         = draw
    , appChooseCursor = BM.showFirstCursor
    , appHandleEvent  = handleEvent
    , appStartEvent   = startEvent
    , appAttrMap      = const $ BA.attrMap
      VA.defAttr
      [ ("selected"       , BU.bg selectedColor)
      , ("currentPath"    , BU.fg currentPathColor)
      , ("sortPatternText", BU.fg sortPatternTextColor)
      ]
    }

-- |Gets encryption key from environment or user and memoizes it for subsequent calls.
getEncryptionKey :: IO EncryptionKey
getEncryptionKey = join $ Memoize.once getEncryptionKey_

getEncryptionKey_ :: IO EncryptionKey
getEncryptionKey_ = do
  ekey <- IOUtils.getEnvWithPromptFallback
    "STASH_ENCRYPTION_KEY"
    "Enter encryption key: "
    True
    False
  valid <- DB.checkEncryptionKey ekey
  if valid then return ekey else die "☠️  Encryption key is invalid for current stash database."

getEncryptionKeyWithConfirmation :: IO EncryptionKey
getEncryptionKeyWithConfirmation = do
  ekey <- IOUtils.getEnvWithPromptFallback "STASH_ENCRYPTION_KEY" "Enter encryption key: " True True
  unless (T.length ekey <= Cipher.maxEncryptionKeyLength)
    $  die
    $  "☠️  Sorry, encryption key can currently be upto "
    ++ show Cipher.maxEncryptionKeyLength
    ++ " characters."
  return ekey

pathWidget :: AppState -> BT.Widget ResourceName
pathWidget state = BWC.withAttr "currentPath" $ BWC.txtWrap $ T.append ">> " $ T.intercalate
  " > "
  (_currentPath state)

keysList :: [PlainKey] -> BWL.GenericList ResourceName Vec.Vector PlainKey
keysList keys = BWL.list "keys" (Vec.fromList keys) 1

listWidget :: AppState -> BT.Widget ResourceName
listWidget s = BWL.renderList f True $ _keysList s
 where
  f True  k = BWC.withAttr "selected" $ f False k
  f False k = BWC.txt $ T.justifyLeft padSize ' ' k
   where
    plainNodes   = _plainNodes s
    minKeyLength = 20
    padSize      = max minKeyLength $ foldr (max . T.length . __key) 0 plainNodes

inputWidget :: AppState -> BT.Widget ResourceName
inputWidget s@AppState { _sortPatternEditor = editor, _uiMode = SORT } = BWC.hBox
  [BWC.txt "Sort pattern: ", BWE.renderEditor f True editor]
  where f xs = BWC.withAttr "sortPatternText" $ BWC.txt $ T.concat xs
inputWidget s@AppState { _genericEditor = editor, _genericEditPrompt = prompt, _uiMode = GENERIC_EDIT _ }
  = BWC.hBox [BWC.txt prompt, BWE.renderEditor f True editor]
  where f xs = BWC.withAttr "genericEditText" $ BWC.txt $ T.concat xs
inputWidget _ = BWC.txt ""

helpKeys :: T.Text
helpKeys = Table.tabl Table.EnvAscii hdecor vdecor aligns cells
 where
  hdecor = Table.DecorUnion [Table.DecorOnly [1]]
  vdecor = Table.DecorNone
  aligns = [Table.AlignLeft, Table.AlignLeft]
  cells =
    [ ["Shortcuts", "Description"]
    , ["ESC (q)", "Quit"]
    , ["?", "Show this help"]
    , ["+", "Add a new key"]
    , [">", "Add a child key to selected key"]
    , ["-", "Delete selected key"]
    , [", (r)", "Rename selected key"]
    , ["/", "Search and sort by pattern"]
    , ["y", "Copy value of selected key into system clipboard"]
    , ["Enter", "Set value of selected key"]
    , ["H", "See history of values of selected key"]
    , ["Left arrow (h)", "Move to parent of selected key"]
    , ["Right arrow (l)", "Move to child of selected key"]
    , ["Up arrow (k, Ctrl-p)", "Select above"]
    , ["Down arrow (j, Ctrl-n)", "Select below"]
    , ["g, G", "Move to top, Move to bottom"]
    , ["Ctrl-u, Ctrl-d", "Scroll keys half page up / down"]
    , ["Ctrl-b, Ctrl-f", "Scroll keys one page up / down"]
    ]

helpWidget :: BT.Widget ResourceName
helpWidget = BWC.txt helpKeys

mainFrame :: AppState -> BT.Widget ResourceName
mainFrame s = BWC.withBorderStyle BWBS.unicode $ BWB.borderWithLabel (BWC.txt "Stash") $ BWC.vBox
  [ BWC.vLimitPercent 60 $ BWC.hBox
    [ BWC.hLimitPercent 30 $ BWC.padRight (BT.Pad 1) leftWidget
    , BWB.vBorder
    , BWC.hLimitPercent 70 $ BWC.padLeft (BT.Pad 1) rightWidget
    ]
  , BWB.hBorder
  , BWC.txtWrap selectedNodeValue
  , BWC.fill ' '
  , inputWidget s
  ]
 where
  plainNodes = _plainNodes s
  isEmpty    = null plainNodes
  kw         = if isEmpty
    then BWC.vBox
      [ BWC.txt "Stash is Empty!"
      , BWC.padTop (BT.Pad 1) $ BWC.txtWrap "Get started by following instructions on the right."
      ]
    else listWidget s
  ckws              = map BWC.txt $ _selectedChildKeys s
  selectPath        = _selectPath s
  si                = if null selectPath then -1 else snd $ last selectPath
  selectedNodeValue = if isEmpty then "" else __value $ plainNodes !! si
  leftWidget        = BWC.vBox [kw]
  rightWidget       = if isEmpty || _uiMode s == BROWSE_HELP
    then if isEmpty then helpWidget else BWC.vBox [BWC.txt "< Press ESC to hide >", helpWidget]
    else BWC.vBox [pathWidget s, BWC.vBox ckws]

draw :: AppState -> [BT.Widget ResourceName]
draw s = [mainFrame s]

handleArrowKey :: DirectionKey -> AppState -> IO AppState
handleArrowKey RIGHT     state = switchRight state
handleArrowKey LEFT      state = switchLeft state
handleArrowKey direction state = do
  let plainNodes = _plainNodes state
  let n          = length plainNodes
  let increment i = if i < n - 1 then i + 1 else i
  let decrement i = if i > 0 then i - 1 else i
  let selectPath  = _selectPath state
  let ekey        = _ekey state
  let (cpid, osi) = last selectPath
  let
    si = case direction of
      UP   -> decrement osi
      DOWN -> increment osi
      _    -> osi
  selectedChildKeys <- getSelectedChildKeys ekey plainNodes si
  currentPath       <- getCurrentPath ekey plainNodes si
  return $ state
    { _selectedChildKeys = selectedChildKeys
    , _selectPath        = init selectPath ++ [(cpid, si)]
    , _currentPath       = currentPath
    , _keysList          = BWL.listMoveTo si $ keysList $ map __key plainNodes
    }

handleSharedEvent
  :: AppState -> BT.BrickEvent ResourceName e -> BT.EventM ResourceName (BT.Next AppState)
handleSharedEvent s event@(BT.VtyEvent e) = case e of
  V.EvKey V.KEsc        [] -> BM.halt s
  V.EvKey (V.KChar 'q') [] -> BM.halt s
  V.EvKey (V.KChar '?') [] -> BM.continue $ s { _uiMode = BROWSE_HELP }
  _                        -> BM.continue s

getSelected :: AppState -> (PlainNode, ParentId, SelectedIndex)
getSelected s = (plainNodes !! si, pid, si)
 where
  plainNodes = _plainNodes s
  selectPath = _selectPath s
  (pid, si)  = last selectPath

copySelectedValue :: AppState -> IO ()
copySelectedValue s = do
  let (selectedNode, _, _) = getSelected s
  let value                = __value selectedNode
  Hclip.setClipboard $ T.unpack value

editSelectedValue :: AppState -> IO AppState
editSelectedValue s = do
  let ekey                    = _ekey s
  let (selectedNode, pid, si) = getSelected s
  let key                     = __key selectedNode
  let value                   = __value selectedNode
  let
    ext = T.unpack $ if length splits > 1 then last splits else "txt"
      where splits = T.splitOn "." key
  newValue <- IOUtils.edit ext value
  DB.updateNodeValue ekey (__id selectedNode) newValue
  buildState pid si s

showHistoryOfSelectedValue :: AppState -> IO AppState
showHistoryOfSelectedValue s = do
  ekey         <- getEncryptionKey
  userTimeZone <- Time.getCurrentTimeZone
  let
    (selectedNode, _, _) = getSelected s
    modified             = Time.utcToLocalTime userTimeZone $ __modified selectedNode
    path                 = T.append ">> " $ T.intercalate " > " (_currentPath s)

  nodes <- DB.getAllPlainNodeVersions ekey (__id selectedNode)
  let
    history = ":: History for " <> path <> " ::" <> "\n" <> T.intercalate
      "\n"
      [ "▸ " <> T.pack (show modified) <> "\n" <> __value n | n <- nodes ]
  IOUtils.edit "txt" history

  return s

validateGenericEditInput :: AppState -> IO ValidationResult
validateGenericEditInput s@AppState { _uiMode = GENERIC_EDIT GERenameKey } = do
  let (selectedNode, _, _) = getSelected s
  let currentKey           = __key selectedNode
  let newKey = T.concat $ BWE.getEditContents $ _genericEditor s
  let isConflictingKey k = Vec.elem k $ BWL.listElements $ _keysList s
  case newKey of
    x | x == T.empty || x == currentKey -> return VRIgnore
    x | isConflictingKey x              -> return $ VRFailed $ T.concat
      ["Name '", newKey, "' is taken. Try a different name for '", currentKey, "': "]
    _ -> return VRSuccess
validateGenericEditInput s@AppState { _uiMode = GENERIC_EDIT GEAddKey } = do
  let newKey = T.concat $ BWE.getEditContents $ _genericEditor s
  let isConflictingKey k = Vec.elem k $ BWL.listElements $ _keysList s
  case newKey of
    x | x == T.empty -> return VRIgnore
    x | isConflictingKey x ->
      return $ VRFailed $ T.concat ["Name '", newKey, "' is taken. Try a different name: "]
    _ -> return VRSuccess
validateGenericEditInput s@AppState { _uiMode = GENERIC_EDIT GEAddChildKey } = do
  let childKey          = T.concat $ BWE.getEditContents $ _genericEditor s
  let existingChildKeys = _selectedChildKeys s
  let isConflictingKey k = k `elem` existingChildKeys
  case childKey of
    x | x == T.empty -> return VRIgnore
    x | isConflictingKey x ->
      return $ VRFailed $ T.concat ["Name '", childKey, "' is taken. Try a different name: "]
    _ -> return VRSuccess
validateGenericEditInput s@AppState { _uiMode = GENERIC_EDIT GEDeleteKey } = do
  let response = T.concat $ BWE.getEditContents $ _genericEditor s
  if response == "yes" then return VRSuccess else return VRIgnore

renameSelectedKey :: AppState -> IO AppState
renameSelectedKey s@AppState { _uiMode = GENERIC_EDIT GERenameKey } = do
  let newKey = T.concat $ BWE.getEditContents $ _genericEditor s
  let (selectedNode, pid, si) = getSelected s
  let ekey                    = _ekey s
  let nid                     = __id selectedNode
  let value                   = __value selectedNode
  let findSelectedIndex xs x = fromMaybe 0 $ findIndex (\n -> __key n == x) xs
  vresult <- validateGenericEditInput s
  case vresult of
    VRSuccess -> do
      DB.updateNode ekey nid newKey value
      newState <- buildState pid si $ switchToBrowseMode s
      let newPlainNodes = _plainNodes newState
      moveKeysList newState (findSelectedIndex newPlainNodes newKey)
    VRIgnore         -> return $ switchToBrowseMode s
    VRFailed message -> return s { _genericEditPrompt = message }

addKey :: AppState -> IO AppState
addKey s@AppState { _uiMode = GENERIC_EDIT GEAddKey } = do
  let newKey = T.concat $ BWE.getEditContents $ _genericEditor s
  let isEmpty = null $ _plainNodes s
  let pid = if isEmpty then 0 else pid where (_, pid, _) = getSelected s
  let ekey    = _ekey s
  let value   = T.empty
  let findSelectedIndex xs x = fromMaybe 0 $ findIndex (\n -> __key n == x) xs
  vresult <- validateGenericEditInput s
  case vresult of
    VRSuccess -> do
      DB.addNode ekey pid newKey value
      newState <- if isEmpty then buildInitialState else buildState pid 0 $ switchToBrowseMode s
      let newPlainNodes = _plainNodes newState
      moveKeysList newState (findSelectedIndex newPlainNodes newKey)
    VRIgnore         -> return $ switchToBrowseMode s
    VRFailed message -> return s { _genericEditPrompt = message }

addChildKey :: AppState -> IO AppState
addChildKey s@AppState { _uiMode = GENERIC_EDIT GEAddChildKey } = do
  let childKey = T.concat $ BWE.getEditContents $ _genericEditor s
  let (selectedNode, pid, _) = getSelected s
  let nid                    = __id selectedNode
  let ekey                   = _ekey s
  let key                    = __key selectedNode
  let value                  = T.empty
  let findSelectedIndex xs x = fromMaybe 0 $ findIndex (\n -> __key n == x) xs
  vresult <- validateGenericEditInput s
  case vresult of
    VRSuccess -> do
      DB.addNode ekey nid childKey value
      newState <- buildState pid 0 $ switchToBrowseMode s
      let newPlainNodes = _plainNodes newState
      moveKeysList newState (findSelectedIndex newPlainNodes key)
    VRIgnore         -> return $ switchToBrowseMode s
    VRFailed message -> return s { _genericEditPrompt = message }

deleteKey :: AppState -> IO AppState
deleteKey s@AppState { _uiMode = GENERIC_EDIT GEDeleteKey } = do
  vresult <- validateGenericEditInput s
  case vresult of
    VRSuccess -> do
      let (selectedNode, pid, sid) = getSelected s
      let selectPath               = _selectPath s
      let nid                      = __id selectedNode
      idsToDelete <- DB.getIds nid
      DB.deleteNodes idsToDelete
      let newSelectedIndex = max 0 $ sid - 1
      buildState pid newSelectedIndex
        $ switchToBrowseMode s { _selectPath = init selectPath ++ [(pid, newSelectedIndex)] }
    _ -> return $ switchToBrowseMode s

prepareForRenameKey :: AppState -> AppState
prepareForRenameKey s = s
  { _uiMode            = GENERIC_EDIT GERenameKey
  , _genericEditPrompt = prompt
  , _genericEditor     = editor
  }
 where
  (selectedNode, _, _) = getSelected s
  k                    = __key selectedNode
  prompt               = T.concat ["Rename '", k, "' to: "]
  editor               = BWE.editor "genericEditor" (Just 1) ""

prepareForAddKey :: AppState -> AppState
prepareForAddKey s = s
  { _uiMode            = GENERIC_EDIT GEAddKey
  , _genericEditPrompt = prompt
  , _genericEditor     = editor
  }
 where
  prompt = "New key: "
  editor = BWE.editor "genericEditor" (Just 1) ""

prepareForAddChildKey :: AppState -> AppState
prepareForAddChildKey s = s
  { _uiMode            = GENERIC_EDIT GEAddChildKey
  , _genericEditPrompt = prompt
  , _genericEditor     = editor
  }
 where
  prompt = "New child key: "
  editor = BWE.editor "genericEditor" (Just 1) ""

prepareForDeleteKey :: AppState -> AppState
prepareForDeleteKey s = s
  { _uiMode            = GENERIC_EDIT GEDeleteKey
  , _genericEditPrompt = prompt
  , _genericEditor     = editor
  }
 where
  prompt = "Are you sure you want to delete selected key? Type 'yes' to delete: "
  editor = BWE.editor "genericEditor" (Just 1) ""

haltBrick :: AppState -> BT.EventM n (BT.Next AppState)
haltBrick s = do
  liftIO $ do
    wipeClipboard <- IOUtils.getEnvWithDefault "STASH_WIPE_CLIPBOARD_AFTER_BROWSE" "false"
    when (wipeClipboard == "true") $ Hclip.setClipboard ""
  BM.halt s

handleEvent :: AppState -> BT.BrickEvent ResourceName e -> BT.EventM ResourceName (BT.Next AppState)
handleEvent s@AppState { _uiMode = BROWSE_EMPTY } event@(BT.VtyEvent e) = case e of
  V.EvKey (V.KChar '+') [] -> BM.continue $ prepareForAddKey s
  V.EvKey (V.KChar '?') [] -> BM.continue s
  _                        -> handleSharedEvent s event
handleEvent s@AppState { _uiMode = BROWSE_HELP } event@(BT.VtyEvent e) = case e of
  V.EvKey V.KEsc [] -> BM.continue $ switchToBrowseMode s
  _                 -> handleSharedEvent s event
handleEvent s@AppState { _uiMode = BROWSE } event@(BT.VtyEvent e) = case e of
  V.EvKey V.KEsc        []        -> haltBrick s
  V.EvKey V.KEnter      []        -> BM.suspendAndResume $ editSelectedValue s
  V.EvKey V.KLeft       []        -> move s LEFT
  V.EvKey V.KRight      []        -> move s RIGHT
  V.EvKey V.KUp         []        -> move s UP
  V.EvKey V.KDown       []        -> move s DOWN
  V.EvKey (V.KChar 'p') [V.MCtrl] -> move s UP
  V.EvKey (V.KChar 'n') [V.MCtrl] -> move s DOWN
  V.EvKey (V.KChar 'h') []        -> move s LEFT
  V.EvKey (V.KChar 'l') []        -> move s RIGHT
  V.EvKey (V.KChar 'k') []        -> move s UP
  V.EvKey (V.KChar 'j') []        -> move s DOWN
  V.EvKey (V.KChar 'g') []        -> moveToTop s
  V.EvKey (V.KChar 'G') []        -> moveToBottom s
  V.EvKey (V.KChar 'f') [V.MCtrl] -> movePageDown s
  V.EvKey (V.KChar 'b') [V.MCtrl] -> movePageUp s
  V.EvKey (V.KChar 'd') [V.MCtrl] -> moveByPages s (0.5 :: Double)
  V.EvKey (V.KChar 'u') [V.MCtrl] -> moveByPages s (-0.5 :: Double)
  V.EvKey (V.KChar '/') []        -> BM.continue s { _uiMode = SORT }
  V.EvKey (V.KChar 'r') []        -> BM.continue $ prepareForRenameKey s
  V.EvKey (V.KChar ',') []        -> BM.continue $ prepareForRenameKey s
  V.EvKey (V.KChar '+') []        -> BM.continue $ prepareForAddKey s
  V.EvKey (V.KChar '>') []        -> BM.continue $ prepareForAddChildKey s
  V.EvKey (V.KChar '-') []        -> BM.continue $ prepareForDeleteKey s
  V.EvKey (V.KChar 'y') []        -> liftIO (copySelectedValue s) >> BM.continue s
  V.EvKey (V.KChar 'H') []        -> BM.suspendAndResume $ showHistoryOfSelectedValue s
  _                               -> handleSharedEvent s event
 where
  move s direction = do
    newState <- liftIO $ handleArrowKey direction s
    BM.continue newState
  moveToTop s = moveToIndex s 0
  moveToBottom s = do
    let si = max 0 $ length (_plainNodes s) - 1
    moveToIndex s si
  movePageDown s = do
    newKeysList <- BWL.listMovePageDown $ _keysList s
    moveToSelected s newKeysList
  movePageUp s = do
    newKeysList <- BWL.listMovePageUp $ _keysList s
    moveToSelected s newKeysList
  moveByPages s degree = do
    newKeysList <- BWL.listMoveByPages degree $ _keysList s
    moveToSelected s newKeysList
  moveToSelected s newKeysList = do
    let selected = BWL.listSelected newKeysList
    case selected of
      Just si -> moveToIndex s si
      Nothing -> BM.continue s
  moveToIndex s si = do
    newState <- liftIO $ moveKeysList s si
    BM.continue newState
handleEvent s@AppState { _uiMode = SORT, _sortPatternEditor = editor } event@(BT.VtyEvent e) =
  case e of
    V.EvKey V.KEsc   [] -> BM.continue $ switchToBrowseMode s
    V.EvKey V.KEnter [] -> BM.continue $ switchToBrowseMode s
    _                   -> do
      editor   <- BWE.handleEditorEvent e editor
      newState <- liftIO $ moveKeysList s { _sortPatternEditor = editor } 0
      BM.continue newState
handleEvent s@AppState { _uiMode = GENERIC_EDIT GERenameKey, _genericEditor = editor } event@(BT.VtyEvent e)
  = case e of
    V.EvKey V.KEsc   [] -> BM.continue $ switchToBrowseMode s
    V.EvKey V.KEnter [] -> do
      newState <- liftIO $ renameSelectedKey s
      BM.continue newState
    _ -> do
      editor <- BWE.handleEditorEvent e editor
      BM.continue s { _genericEditor = editor }
handleEvent s@AppState { _uiMode = GENERIC_EDIT GEAddKey, _genericEditor = editor } event@(BT.VtyEvent e)
  = case e of
    V.EvKey V.KEsc   [] -> BM.continue $ switchToBrowseMode s
    V.EvKey V.KEnter [] -> do
      newState <- liftIO $ addKey s
      BM.continue newState
    _ -> do
      editor <- BWE.handleEditorEvent e editor
      BM.continue s { _genericEditor = editor }
handleEvent s@AppState { _uiMode = GENERIC_EDIT GEAddChildKey, _genericEditor = editor } event@(BT.VtyEvent e)
  = case e of
    V.EvKey V.KEsc   [] -> BM.continue $ switchToBrowseMode s
    V.EvKey V.KEnter [] -> do
      newState <- liftIO $ addChildKey s
      BM.continue newState
    _ -> do
      editor <- BWE.handleEditorEvent e editor
      BM.continue s { _genericEditor = editor }
handleEvent s@AppState { _uiMode = GENERIC_EDIT GEDeleteKey, _genericEditor = editor } event@(BT.VtyEvent e)
  = case e of
    V.EvKey V.KEsc   [] -> BM.continue $ switchToBrowseMode s
    V.EvKey V.KEnter [] -> do
      newState <- liftIO $ deleteKey s
      BM.continue newState
    _ -> do
      editor <- BWE.handleEditorEvent e editor
      BM.continue s { _genericEditor = editor }

switchToBrowseMode :: AppState -> AppState
switchToBrowseMode s = s { _uiMode = mode }
  where mode = if (null . _plainNodes) s then BROWSE_EMPTY else BROWSE

moveKeysList :: AppState -> SelectedIndex -> IO AppState
moveKeysList s si = do
  let selectPath = _selectPath s
  let (pid, _)   = last selectPath
  buildState pid si $ s { _selectPath = init selectPath ++ [(pid, si)] }
  buildState pid si $ setSelectedIndex s si

sortPlainNodes :: [PlainNode] -> T.Text -> [PlainNode]
sortPlainNodes ns p = sortBy cmp ns
 where
  score k = case match k of
    Nothing                     -> 0
    Just TF.Fuzzy { score = n } -> n
  cmp PlainNode { __key = k1 } PlainNode { __key = k2 } =
    if T.null p then compare k1 k2 else compare (score k2) (score k1)
  match k = TF.match p k "" "" id False

startEvent :: AppState -> BT.EventM ResourceName AppState
startEvent = return

getSelectedChildKeys :: EncryptionKey -> [PlainNode] -> SelectedIndex -> IO [PlainKey]
getSelectedChildKeys ekey plainNodes si = do
  if null plainNodes
    then return []
    else do
      let pid = __id $ plainNodes !! si
      DB.getPlainKeys ekey pid

getCurrentPath :: EncryptionKey -> [PlainNode] -> SelectedIndex -> IO [PlainKey]
getCurrentPath ekey plainNodes si = do
  if null plainNodes
    then return []
    else do
      let nid = __id $ plainNodes !! si
      DB.getPath ekey nid

buildInitialState :: IO AppState
buildInitialState = do
  ekey       <- getEncryptionKey
  plainNodes <- DB.getPlainNodes ekey 0
  let sortedPlainNodes = sortPlainNodes plainNodes T.empty
  let si               = if null sortedPlainNodes then -1 else 0
  selectedChildKeys <- getSelectedChildKeys ekey sortedPlainNodes si
  currentPath       <- getCurrentPath ekey sortedPlainNodes si
  return AppState
    { _plainNodes        = sortedPlainNodes
    , _ekey              = ekey
    , _selectedChildKeys = selectedChildKeys
    , _selectPath        = [ (0, si) | not (null sortedPlainNodes) ]
    , _currentPath       = currentPath
    , _keysList          = keysList $ map __key sortedPlainNodes
    , _uiMode            = if null sortedPlainNodes then BROWSE_EMPTY else BROWSE
    , _sortPatternEditor = BWE.editor "sortPatternEditor" (Just 1) ""
    , _genericEditor     = BWE.editor "genericEditor" (Just 1) ""
    , _genericEditPrompt = T.empty
    }

buildState :: ParentId -> SelectedIndex -> AppState -> IO AppState
buildState pid si state = do
  let ekey            = _ekey state
  let sortPatternText = T.concat $ BWE.getEditContents $ _sortPatternEditor state
  plainNodes <- DB.getPlainNodes ekey pid
  let sortedPlainNodes = sortPlainNodes plainNodes sortPatternText
  selectedChildKeys <- getSelectedChildKeys ekey sortedPlainNodes si
  currentPath       <- getCurrentPath ekey sortedPlainNodes si
  return state
    { _plainNodes        = sortedPlainNodes
    , _selectedChildKeys = selectedChildKeys
    , _currentPath       = currentPath
    , _keysList          = BWL.listMoveTo si $ keysList $ map __key sortedPlainNodes
    }

switchLeft :: AppState -> IO AppState
switchLeft s = do
  let (newState, pid, si) = popSelectPath s
  buildState pid si newState

switchRight :: AppState -> IO AppState
switchRight s = do
  if null $ _selectedChildKeys s
    then return s
    else do
      let (selectedNode, _, _) = getSelected s
      let pid                  = __id selectedNode
      buildState pid 0 $ pushSelectPath s pid 0

pushSelectPath :: AppState -> ParentId -> SelectedIndex -> AppState
pushSelectPath s pid si = s { _selectPath = newSelectPath }
 where
  currentSelectPath = _selectPath s
  newSelectPath     = currentSelectPath ++ [(pid, 0)]

popSelectPath :: AppState -> (AppState, ParentId, SelectedIndex)
popSelectPath s = (s { _selectPath = newSelectPath }, pid, si)
 where
  selectPath    = _selectPath s
  newSelectPath = if length selectPath == 1 then selectPath else init selectPath
  (pid, si)     = last newSelectPath

setSelectedIndex :: AppState -> SelectedIndex -> AppState
setSelectedIndex s si = s { _selectPath = init selectPath ++ [(pid, si)] }
 where
  selectPath = _selectPath s
  (pid, _)   = last selectPath

browseTUI :: IO ()
browseTUI = do
  initialState <- buildInitialState
  app          <- buildBrickApp
  void $ BM.defaultMain app initialState

printDiff :: T.Text -> T.Text -> IO ()
printDiff t1 t2 = do
  let diffs = Diff.getGroupedDiff (T.lines t1) (T.lines t2)

  let
    toDoc xs = PP.text $ T.unpack $ T.unlines xs
    pdiff ((Diff.Both xs _) : ys) = do
      if length xs <= 2 then PP.putDoc $ toDoc xs else TIO.putStrLn "..."
      pdiff ys
    pdiff ((Diff.First xs) : ys) = do
      PP.putDoc $ PP.red $ "-" <> toDoc xs <> PP.linebreak
      pdiff ys
    pdiff ((Diff.Second xs) : ys) = do
      PP.putDoc $ PP.green $ "+" <> toDoc xs <> PP.linebreak
      pdiff ys
    pdiff [] = return ()

  pdiff diffs

deleteInteractively :: [PlainNode] -> Bool -> IO ()
deleteInteractively []       _     = return ()
deleteInteractively (n : ns) False = do
  DB.deleteNodes [__id n]
  deleteInteractively ns False
deleteInteractively (n : ns) True = do
  let nid = __id n
  ekey <- getEncryptionKey
  keys <- DB.getPath ekey nid
  TIO.putStrLn $ T.concat
    ["The following item was marked for deletion:\n> ", T.intercalate " > " keys, "\n", __value n]
  userResponse <- IOUtils.readUserResponseYesNo "Are you sure you want to delete?"
  case userResponse of
    IOUtils.URYes -> do
      DB.deleteNodes [nid]
      deleteInteractively ns True
    IOUtils.URNo       -> putStrLn "Ok, skipping delete."
    IOUtils.URNoToAll  -> putStrLn "Ok, skipping any more deletes."
    IOUtils.URYesToAll -> deleteInteractively (n : ns) False

browseText :: TextFormat -> IO ()
browseText format = do
  ekey       <- getEncryptionKey
  plainNodes <- DB.getAllPlainNodes ekey
  let nodeIds = [ __id n | n <- plainNodes ]
  oldText             <- IOUtils.logTime "toText" $ pure $ TextTransform.toText format plainNodes

  lastUserResponseRef <- IORef.newIORef IOUtils.URNo

  newText             <- case format of
    OrgText      -> IOUtils.edit "org" oldText
    MarkdownText -> IOUtils.edit "md" oldText
  let
    lmap = HM.fromList [ ((__parent n, __key n), n) | n <- plainNodes ]
    lookup pid (k : ks) = case HM.lookup (pid, k) lmap of
      Nothing -> Nothing
      Just n  -> if null ks then Just n else lookup (__id n) ks
    walker :: [NodeId] -> [PlainKey] -> PlainValue -> IO [NodeId]
    walker nids ks body = do
      case lookup 0 ks of
        Nothing -> do
          ids <- DB.save ekey ks body
          return $ ids ++ nids
        Just n -> if __value n /= body
          then do
            lastUserResponse <- IORef.readIORef lastUserResponseRef
            case lastUserResponse of
              IOUtils.URYesToAll -> do
                ids <- DB.save ekey ks body
                return $ ids ++ nids
              IOUtils.URNoToAll -> return nodeIds
              _                 -> do
                let oldBody = __value n
                TIO.putStrLn $ T.concat ["▸ Value of [", T.intercalate " > " ks, "] differ:"]
                printDiff oldBody body

                userResponse <- IOUtils.readUserResponseYesNo "Accept this change?"
                IORef.writeIORef lastUserResponseRef userResponse

                let
                  handleUserResponse r | r `elem` [IOUtils.URYes, IOUtils.URYesToAll] = do
                    ids <- DB.save ekey ks body
                    return $ ids ++ nids
                  handleUserResponse r | r `elem` [IOUtils.URNo, IOUtils.URNoToAll] = do
                    ids <- DB.getIdsInPath ks
                    return $ ids ++ nids

                handleUserResponse userResponse
          else do
            ids <- DB.save ekey ks body
            return $ ids ++ nids

  processedNodeIds <- Set.fromList <$> TextTransform.walkText [] format newText walker
  let deletedNodeIds = Set.fromList [ x | x <- nodeIds, not $ Set.member x processedNodeIds ]
  unless (null deletedNodeIds) $ do
    deleteInteractively [ n | n <- plainNodes, Set.member (__id n) deletedNodeIds ] True

dump :: TextFormat -> IO ()
dump JSONText = do
  ekey       <- getEncryptionKey
  plainTrees <- DB.getPlainTrees ekey 0
  let
    config = AesonPretty.Config
      { confIndent          = AesonPretty.Spaces 2
      , confCompare         = mempty
      , confNumFormat       = AesonPretty.Generic
      , confTrailingNewline = False
      }
  BLC.putStrLn $ AesonPretty.encodePretty' config plainTrees
dump format = do
  ekey       <- getEncryptionKey
  plainNodes <- DB.getAllPlainNodes ekey

  text       <- IOUtils.logTime "toText" $ pure $ TextTransform.toText format plainNodes
  TIO.putStrLn text

backup :: IO ()
backup = do
  source   <- DB.getDBPath
  userTime <- Time.getZonedTime

  let
    destinationDirectory = takeDirectory source
    destinationFileName =
      "backup-" <> takeBaseName source <> "-" <> filter (/= ' ') (show userTime) <> ".stash"
    destination = combine destinationDirectory destinationFileName

  copyFileWithMetadata source destination
  putStrLn $ "Backed up " <> source <> " to " <> destination

initialize :: FilePath -> Bool -> IO ()
initialize path createIfMissing = do
  dbPath           <- DB.setDBPath path
  validationResult <- DB.validateDBPath dbPath
  case validationResult of
    DB.NonExistentDBFile -> do
      unless createIfMissing $ die $ "☠️  stash file " <> dbPath <> " does not exist."
      isDirectory <- doesDirectoryExist dbPath
      when isDirectory
        $  die
        $  "☠️  "
        <> dbPath
        <> " is a directory. Did you mean "
        <> dbPath
        <> ".stash?"
      TIO.putStrLn $ "Creating new stash file " <> T.pack dbPath <> "..."
      ekey <- getEncryptionKeyWithConfirmation
      IOUtils.createMissingDirectories dbPath
      DB.bootstrap ekey
      TIO.putStrLn $ T.intercalate
        "\n"
        [ T.append "Created stash file " $ T.pack dbPath
        , "\nOnly a salted hash (good random salt + SHA512) of the encryption-key was saved."
        , "Stash will prompt for the encryption-key when needed."
        , "(unless STASH_ENCRYPTION_KEY environment variable is set)"
        ]
    DB.InvalidDBFile -> die $ "☠️  Invalid stash file " <> dbPath
    DB.ValidDBFile   -> return ()

processCommand :: C.Command -> IO ()
processCommand (C.CreateCommand dbPath) = initialize dbPath True
processCommand (C.BackupCommand dbPath) = do
  initialize dbPath False
  backup
processCommand (C.BrowseCommand dbPath format) = do
  initialize dbPath False
  case format of
    C.BrowseFormatTUI      -> browseTUI
    C.BrowseFormatMarkdown -> browseText MarkdownText
    C.BrowseFormatOrg      -> browseText OrgText
processCommand (C.DumpCommand dbPath format) = do
  initialize dbPath False
  case format of
    C.DumpFormatJSON     -> dump JSONText
    C.DumpFormatMarkdown -> dump MarkdownText
    C.DumpFormatOrg      -> dump OrgText

setUpLogging :: IO ()
setUpLogging = do
  logLevel <- IOUtils.getEnvWithDefault "STASH_LOG_LEVEL" "INFO"
  case logLevel of
    "DEBUG" -> L.setLogLevel L.LevelDebug
    "INFO"  -> L.setLogLevel L.LevelInfo
    "WARN"  -> L.setLogLevel L.LevelWarn
    _       -> L.setLogLevel L.LevelError

main :: IO ()
main = do
  setUpLogging
  pod <- IOUtils.getEnvWithDefault "BABASHKA_POD" "false"
  case pod of
    "true"  -> BPod.interactWithBencode
    "false" -> do
      cmd <- O.customExecParser preferences opts
      processCommand cmd
 where
  parser = O.subparser $ mconcat C.commands
  opts   = O.info
    (parser O.<**> O.helper)
    (O.fullDesc <> O.progDesc "stash [create | browse | dump | backup]" <> O.header
      ("Stash " <> appVersion <> " https://github.com/rorokimdim/stash")
    )
  preferences = O.prefs (O.showHelpOnError <> O.showHelpOnEmpty)
