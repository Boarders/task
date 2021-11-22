{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

-- base
import Control.Category hiding ((.), id)
-- brick
import Brick
import Brick.Widgets.Center as Brick
import Brick.Widgets.Border
import Brick.Widgets.Edit
import qualified Brick.Widgets.Border.Style as BS


-- text-zipper
import qualified Data.Text.Zipper as TextZipper

-- vty
import Graphics.Vty.Attributes as Vty
import qualified Graphics.Vty.Input as Vty
import qualified Graphics.Vty.Input.Events as Vty
import qualified Graphics.Vty.Image as Vty

-- text
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy (toStrict)

-- containers
import qualified Data.Map as Map

-- chronos
import Chronos

-- lens
import Control.Lens

-- Core
import qualified Task as Task
import Task (Task, _text, _status, _task_uid)
import TaskZipper
--import Cursor
import TaskUID
import Bwd
--import Command
import Date
import LineCursor

import Debug.Trace

main :: IO ()
main = do
  d <- dateToday
  defaultMain (app d) initialState
  pure ()

data ViewMode =
    Status
  | Input
  deriving Show

flipViewMode :: ViewMode -> ViewMode
flipViewMode = \case
  Status -> Input
  Input  -> Status

data CursorLoc = CursorLoc
  deriving stock Eq

data ViewState = ViewState
  { curr_tasks :: TaskZipper
  , view_mode  :: ViewMode
  }

_curr_tasks :: Lens' ViewState TaskZipper
_curr_tasks = lens curr_tasks (\vs ct -> vs {curr_tasks = ct})

_view_mode :: Lens' ViewState ViewMode
_view_mode = lens view_mode (\vs ct -> vs {view_mode = ct})


type WidgetId = Text
type TaskWidget = Widget WidgetId


exampleTasks :: TaskZipper
exampleTasks = TaskZipper (Nil :|> (Task.newTask "first task" Task.Unfinished 0)) (Task.newTask lcEmpty Task.Done 1)
   [Task.newTask "do this, do that" Task.Unfinished 2,
    Task.newTask "do something else, always doing something!" Task.Unfinished 2]


initialState :: ViewState
initialState = ViewState exampleTasks Input


app :: (Ord d, Show d, RenderDate d) => d -> App ViewState () WidgetId
app d = App { appDraw = draw d
            , appChooseCursor = showFirstCursor
            , appHandleEvent = handleEvent
            , appStartEvent = pure
            , appAttrMap = \lc -> edAttrMap
            }

modeKey :: Vty.Key
modeKey = Vty.KChar '@'


handleEvent :: ViewState -> BrickEvent WidgetId () -> EventM WidgetId (Next ViewState)
handleEvent viewState@(ViewState curr_tasks mode) =
  \case
    VtyEvent ve ->
      case ve of
        Vty.EvKey key mods ->
          case (elem Vty.MCtrl mods, key == modeKey, mode) of
            (True, True, _     ) -> continue (over _view_mode flipViewMode viewState)
            (_   , _   , Status) -> statusInput viewState key
            (True, _   , _     ) -> ctrlInput viewState key
            _ -> normalInput viewState key
        _ -> continue viewState
    _ -> continue viewState


statusInput :: ViewState -> Vty.Key -> EventM Text (Next ViewState)
statusInput viewState@(ViewState tz mode) =
  \case
    Vty.KChar  c
      | c == 'n'  -> continue (over _curr_tasks next_task viewState)
    Vty.KChar  c
      | c == 'p'  -> continue (over _curr_tasks prev_task viewState)
    Vty.KEnter
                  -> continue (over (_curr_tasks . _curr_task . _status) Task.flipStatus viewState)
    key ->  continue viewState
  where
    mDo func = continue $ ViewState (over (_curr_task . _text) func tz) mode

ctrlInput :: ViewState -> Vty.Key -> EventM Text (Next ViewState)
ctrlInput viewState@(ViewState tz mode) =
  \case
    Vty.KChar c | c == 'a'  -> mDo lcTextEnd
    Vty.KChar c | c == 'e'  -> mDo lcTextStart
    Vty.KChar c | c == 'k'  -> mDo lcDeleteAll
    Vty.KChar c | c == 'd'  -> halt viewState
    Vty.KChar c | c == ' '  -> continue (over _view_mode flipViewMode viewState)
    key ->  normalInput viewState key
  where
    mDo func = continue $ ViewState (over (_curr_task . _text) func tz) mode
{-    
ctrlInput :: ViewState -> Vty.Key -> EventM WidgetId (Next ViewState)
ctrlInput viewState@(ViewState taskList mode) =
  \case
    Vty.KChar c | c == 'f'  -> mDo (set (_curr_task . Task._status) Task.Done)
    Vty.KChar c | c == 'u'  -> mDo (set (_curr_task . Task._status) Task.Unfinished)
    Vty.KChar c | c == 'k'  -> mDo (set (_curr_task . Task._status) Task.Cancelled)
    Vty.KChar c | c == 'n'  -> mDo next_task
    Vty.KChar c | c == 'p'  -> mDo prev_task
    _ ->  error "to do"
  where
    mDo func = continue $ ViewState (func taskList) mode
-}

normalInput :: ViewState -> Vty.Key -> EventM WidgetId (Next ViewState)
normalInput viewState@(ViewState tasks mode) = do
  \case
--    KChar c | c == '\\' -> continue $ ViewState doc EdSpec
    Vty.KChar c -> mDo $ (lcInsert c)
    Vty.KLeft   -> mDo lcPrev
    Vty.KRight  -> mDo lcNext
    Vty.KBS     -> mDo lcDeleteBackard
    Vty.KDel    -> mDo lcDeleteForward
    Vty.KEsc    -> halt viewState
--    KEnter  -> mDo docNewLine
    _ -> continue viewState
  where
    mDo func = continue $ ViewState (over (_curr_task . _text) func tasks) mode



{-
specialInput :: ViewState d -> Vty.Key -> EventM Text (Next ViewState d)
specialInput viewState@(ViewState d lc mode) key =
  case key of
    KChar c ->
      case Map.lookup c specialInputMap of
        Just spec -> mDo $ (docEdLine (lcAddWord spec))
        Nothing   -> normalInput (viewState{viewMode = Status}) key
    _ -> normalInput viewState key
  where
    mDo func = continue $ ViewState d (func lc) EdOrd
-}
class RenderDate d where
  renderDate :: d -> Text

instance RenderDate Date where
  renderDate :: Date -> Text
  renderDate = toStrict . toLazyText . builder_Dmy (Just '/')


draw :: forall d . (Ord d, Show d, RenderDate d) => d -> ViewState -> [TaskWidget]
draw date (ViewState tasks mode) =
  let
    header, textWindow :: TaskWidget
    header =
      visible $
      withAttr "header" $
      vLimitPercent 5 $
--      borderWithLabel emptyWidget $
--      viewport "header"  Both $
--      setAvailableSize (100, 100) $
      center $   
      border $
--      padAll 1 $
      visible $
      withAttr "subtitle" $
      txt (renderDate $ date)
    textWindow =
      borderWithLabel (withAttr "title" $ txt "To do") $
      withAttr "to-do-list" $
      viewport (renderDate date) Both $
      setAvailableSize (100, 800) $      
      visible $
      drawTasks mode $
      tasks
    myWidget = withAttr "highlight" $
      center $
          header
      <=> textWindow
  in

    [ myWidget
    ]

taskBorder = joinBorders . withBorderStyle BS.unicode

space = setAvailableSize (100, 10) emptyWidget

drawTasks :: ViewMode -> TaskZipper -> Widget WidgetId
drawTasks viewMode (TaskZipper bwd ct tsks) =
  vBox
    [ drawPrevTasks bwd
    , drawCurrTask viewMode ct
    , drawNextTasks tsks
    ]
-- 〚✓〛✗

renderTaskStatus :: Task.Status -> Text
renderTaskStatus = \case
  Task.Done -> "☒  "
  Task.Unfinished ->  "☐  "

selectViewAttr :: ViewMode -> (Widget a -> Widget a)
selectViewAttr = \case
  Status -> withAttr "highlighted"
  Input  -> id


drawCurrTask :: ViewMode -> CurrTask -> Widget WidgetId
drawCurrTask viewMode currTask =
    selectViewAttr viewMode $
    showCursor "cursor" (Location (currTask & (view _text >>> lcTextWidth >>> (+ startCol)), 0))
    (currTask & (view _text >>> lcToText >>> (statusTxt <>) >>> txt))
  where
    startCol = Text.length statusTxt
    statusTxt = currTask & (view _status >>> renderTaskStatus)


drawTask :: Task -> Widget WidgetId
drawTask t =
  txt $ statusTxt <> view _text t
  where
    statusTxt = t & (view _status >>> renderTaskStatus)  

drawPrevTasks :: Bwd Task -> Widget WidgetId
drawPrevTasks = foldr (\t w -> drawTask t <=> w) emptyWidget

drawNextTasks :: [Task] -> Widget WidgetId
drawNextTasks = foldr (\t w -> drawTask t <=> w) emptyWidget


edAttrMap :: AttrMap
edAttrMap =
  attrMap globalDefault
    [ ("title"   , titleCol    `on` backgroundCol)
    , ("subtitle", subtitleCol `on` backgroundCol)
    , ("text"    , textCol     `on` backgroundCol)
    , ("to-do-list", textCol   `on` backgroundCol)
    , ("highlighted", textCol  `on` highlightCol)
    , (borderAttr, textCol     `on` borderCol)
    ]

globalDefault :: Attr
globalDefault = textCol `on` backgroundCol

-- colour scheme
type Colour = Color

textCol :: Colour
textCol = richBlack

backgroundCol :: Color
backgroundCol = babyPowder

headerBackground :: Color
headerBackground = lightPurple

headerCol :: Color
headerCol = violet

checkBackgroundCol :: Color
checkBackgroundCol = tiffanyBlue

titleCol :: Color
titleCol = orangePeel

subtitleCol :: Color
subtitleCol = violet

highlightCol :: Color
highlightCol = lightPurple

borderCol :: Color
borderCol = backgroundCol

richBlack :: Colour
richBlack = Vty.rgbColor 1 22 39

babyPowder :: Colour
babyPowder = Vty.rgbColor 253 255 252

tiffanyBlue :: Colour
tiffanyBlue = Vty.rgbColor 46 196 182

roseMadder :: Colour
roseMadder = Vty.rgbColor 231 29 54

orangePeel :: Colour
orangePeel = Vty.rgbColor 255 94 14

violet :: Colour
violet = Vty.rgbColor 143 0 255

lightPurple :: Colour
lightPurple = Vty.rgbColor 216 191 216

lightOrange :: Colour
lightOrange = Vty.rgbColor 254 216 177

todoBackground :: Colour
todoBackground = Vty.rgbColor 255 228 181


accentTone :: Colour
accentTone = Vty.rgbColor 205 133 63
