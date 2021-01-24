{- HLINT ignore "Use camelCase" -}

module Main where

import           Data.Char                           (isSpace)
import           Data.Default                        (def)
import           Data.List                           (dropWhileEnd)
import qualified Data.Map                            as M
import           XMonad
import           XMonad.Actions.CopyWindow           (copyToAll,
                                                      killAllOtherCopies)
import           XMonad.Config.Desktop               (desktopConfig)
import           XMonad.Config.Prime                 (Query)
import           XMonad.Hooks.DynamicLog             (PP (PP), dynamicLogWithPP,
                                                      ppOrder, ppOutput, ppSep,
                                                      ppTitle)
import           XMonad.Hooks.InsertPosition         (Focus (..), Position (..),
                                                      insertPosition)
import           XMonad.Hooks.ManageDocks            (AvoidStruts,
                                                      ToggleStruts (ToggleStruts),
                                                      avoidStruts, docks)
import           XMonad.Hooks.RefocusLast            (refocusLastLayoutHook,
                                                      refocusLastWhen,
                                                      refocusingIsActive)
import           XMonad.Layout                       (IncMasterN (IncMasterN),
                                                      Resize (Expand, Shrink))
import           XMonad.Layout.LayoutModifier        (ModifiedLayout (ModifiedLayout))
import qualified XMonad.Layout.Magnifier             as Mag
import           XMonad.Layout.MultiToggle           (Toggle (Toggle), mkToggle,
                                                      single)
import           XMonad.Layout.MultiToggle.Instances (StdTransformers (FULL))
import           XMonad.Layout.NoBorders             (smartBorders)
import           XMonad.Layout.Reflect               (reflectHoriz)
import           XMonad.Layout.ResizableTile         (MirrorResize (MirrorExpand, MirrorShrink),
                                                      ResizableTall (ResizableTall))
import           XMonad.Layout.Spacing               (Border (Border),
                                                      spacingRaw)
import qualified XMonad.StackSet                     as W
import           XMonad.Util.Run                     (hPutStrLn, spawnPipe)

-- Blackbird operator for composition over two arguments
($.) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
($.) = (.) . (.)

-- Predicate and two branches on two arguments
if2 :: (a -> b -> Bool) -> (a -> b -> c) -> (a -> b -> c) -> a -> b -> c
if2 p f g x y = if p x y then f x y else g x y

trimStart :: String -> String
trimStart = dropWhile isSpace

trimEnd :: String -> String
trimEnd = dropWhileEnd isSpace

trim :: String -> String
trim = trimEnd . trimStart

barCmd :: String
barCmd = "xmobar-samhh"

nomod :: KeyMask
nomod = 0

xK_VolDown :: KeySym
xK_VolDown = 0x1008FF11

xK_VolUp :: KeySym
xK_VolUp = 0x1008FF13

xK_ToggleMute :: KeySym
xK_ToggleMute = 0x1008FF12

xK_MediaPrev :: KeySym
xK_MediaPrev = 0x1008FF16

xK_MediaTogglePlay :: KeySym
xK_MediaTogglePlay = 0x1008FF14

xK_MediaNext :: KeySym
xK_MediaNext = 0x1008FF17

type HexColor = String

nord0 :: HexColor
nord0 = "#2e3440"

nord1 :: HexColor
nord1 = "#3b4252"

nord2 :: HexColor
nord2 = "#434c5e"

nord3 :: HexColor
nord3 = "#4c556a"

nord4 :: HexColor
nord4 = "#d8dee9"

nord5 :: HexColor
nord5 = "#e5e9f0"

nord6 :: HexColor
nord6 = "#eceff4"

nord11 :: HexColor
nord11 = "#bf616a"

-- | Like `XMonad.Hooks.DynamicLog::statusBar`, but doesn't require a toggle
-- hotkey.
createStatusBarKeyless ::
  LayoutClass l Window =>
  String ->
  PP ->
  XConfig l ->
  IO (XConfig (ModifiedLayout AvoidStruts l))
createStatusBarKeyless cmd pp cfg = do
  h <- spawnPipe cmd
  pure $
    docks $
      cfg
        { layoutHook = avoidStruts (layoutHook cfg),
          logHook = do
            logHook cfg
            dynamicLogWithPP pp {ppOutput = hPutStrLn h}
        }

statusBar ::
  LayoutClass a Window =>
  XConfig a ->
  IO (XConfig (ModifiedLayout AvoidStruts a))
statusBar =
  createStatusBarKeyless barCmd $
    def
      { ppOrder = \(w : _ : t : _) -> [w, t],
        ppSep = " | ",
        ppTitle = limit . trim
      }
  where
    limit :: String -> String
    limit xs
      | length xs > 75 = take 72 xs <> "..."
      | otherwise = xs

type Workspace = (String, KeySym)

ws :: [Workspace]
ws =
  [ ("1", xK_1),
    ("2", xK_2),
    ("3", xK_3),
    ("4", xK_4),
    ("5", xK_5),
    ("6", xK_6),
    ("7", xK_7),
    ("8", xK_8),
    ("9", xK_9),
    ("0", xK_0)
  ]

wsName :: Workspace -> String
wsName = fst

wsView :: KeyMask -> Workspace -> ((KeyMask, KeySym), X ())
wsView super (name, k) =
  let x = windows $ W.greedyView name
   in ((super, k), x)

wsSwitch :: KeyMask -> Workspace -> ((KeyMask, KeySym), X ())
wsSwitch super (name, k) =
  let x = windows $ W.shift name
   in ((super .|. shiftMask, k), x)

centreRect :: W.RationalRect
centreRect = W.RationalRect (1 / 3) (1 / 3) (1 / 3) (1 / 3)

videoRect :: W.RationalRect
videoRect = W.RationalRect offset offset size size
  where
    size = 1 / 4
    offset = 1 - size - (size / 8)

isFloating :: Window -> WindowSet -> Bool
isFloating w s = M.member w (W.floating s)

enableFloat :: W.RationalRect -> Window -> (WindowSet -> WindowSet)
enableFloat = flip W.float

enableFloat' :: W.RationalRect -> Window -> X ()
enableFloat' = windows $. enableFloat

disableFloat :: Window -> (WindowSet -> WindowSet)
disableFloat = W.sink

disableFloat' :: Window -> X ()
disableFloat' = windows . disableFloat

toggleFloat :: W.RationalRect -> Window -> X ()
toggleFloat r = windows . if2 isFloating disableFloat (enableFloat r)

toggleFullscreen' :: X ()
toggleFullscreen' = sendMessage (Toggle FULL) <> sendMessage ToggleStruts

layoutName :: Query String
layoutName = liftX . gets $ description . W.layout . W.workspace . W.current . windowset

isFullscreenQuery :: Query Bool
isFullscreenQuery = layoutName =? show Full

data OnFullscreenDestroy
  = Retain
  | Exit

-- | On destroy window event, potentially check if the window is fullscreen and
-- if so toggle it, else refocus the last focused window.
getFullscreenEventHook :: OnFullscreenDestroy -> Event -> X All
getFullscreenEventHook Exit DestroyWindowEvent {ev_window = w, ev_event = evt} = do
  -- The `DestroyWindowEvent` is emitted a lot, the condition verifies it's
  -- actually what we're looking for. See also:
  -- https://github.com/xmonad/xmonad-contrib/blob/4a6bbb63b4e4c470e01a6c81bf168b81952b85d6/XMonad/Hooks/WindowSwallowing.hs#L122
  when (w == evt) $ whenX (runQuery isFullscreenQuery w) toggleFullscreen'
  pure $ All True
getFullscreenEventHook _ evt = refocusLastWhen refocusingIsActive evt

layout = avoidStruts $ smartBorders $ Mag.maximizeVertical $ refocusLastLayoutHook $ mkToggle (single FULL) $ tiled ||| reflectHoriz tiled
  where
    tiled = spacingRaw False gaps True gaps True $ ResizableTall numMaster resizeDelta masterRatio mempty
    gaps = Border 6 6 6 6
    numMaster = 1
    resizeDelta = 3 / 100
    masterRatio = 1 / 2

resetLayout :: XConfig Layout -> X ()
resetLayout = setLayout . layoutHook

main :: IO ()
main =
  (xmonad . docks)
    =<< statusBar
      desktopConfig
        { terminal = "alacritty",
          modMask = mod4Mask,
          focusFollowsMouse = False,
          clickJustFocuses = False,
          manageHook = insertPosition Below Newer,
          handleEventHook = getFullscreenEventHook Exit,
          workspaces = fmap wsName ws,
          borderWidth = 3,
          normalBorderColor = nord0,
          focusedBorderColor = nord3,
          layoutHook = layout,
          keys = \cfg@XConfig {XMonad.modMask = super, XMonad.terminal = term} ->
            M.fromList $
              [ ((super, xK_Return), spawn term),
                ((super .|. shiftMask, xK_q), kill),
                ((super, xK_j), windows W.focusDown),
                ((super, xK_k), windows W.focusUp),
                ((super .|. shiftMask, xK_j), windows W.swapDown),
                ((super .|. shiftMask, xK_k), windows W.swapUp),
                ((super .|. shiftMask, xK_m), windows W.swapMaster),
                ((super, xK_h), sendMessage MirrorShrink <> sendMessage MirrorShrink),
                ((super, xK_l), sendMessage MirrorExpand <> sendMessage MirrorExpand),
                ((super .|. shiftMask, xK_h), sendMessage Shrink),
                ((super .|. shiftMask, xK_l), sendMessage Expand),
                ((super, xK_r), resetLayout cfg),
                ((super, xK_v), sendMessage NextLayout),
                ((super, xK_f), toggleFullscreen'),
                ((super, xK_z), sendMessage Mag.Toggle),
                ((super, xK_q), sendMessage $ IncMasterN (-1)),
                ((super, xK_e), sendMessage $ IncMasterN 1),
                ((super, xK_s), withFocused $ toggleFloat centreRect),
                ((super, xK_a), windows copyToAll <> withFocused (enableFloat' videoRect)),
                ((super .|. shiftMask, xK_a), killAllOtherCopies <> withFocused disableFloat'),
                ((super, xK_o), spawn "dunstctl close"),
                ((super .|. shiftMask, xK_o), spawn "dunstctl close-all"),
                ((nomod, xK_VolDown), spawn "amixer -Mq sset Master 1%-"),
                ((nomod, xK_VolUp), spawn "amixer -Mq sset Master 1%+"),
                ((super, xK_VolDown), spawn "playerctl -p mpv volume 0.05-"),
                ((super, xK_VolUp), spawn "playerctl -p mpv volume 0.05+"),
                ((nomod, xK_ToggleMute), spawn "amixer -q set Master toggle"),
                ((nomod, xK_MediaPrev), spawn "playerctl previous -p mpd"),
                ((nomod, xK_MediaTogglePlay), spawn "playerctl play-pause -p mpd"),
                ((super, xK_MediaTogglePlay), spawn "playerctl play-pause -p mpv"),
                ((nomod, xK_MediaNext), spawn "playerctl next -p mpd"),
                ((super, xK_w), spawn "systemctl --user start wallpaper"),
                -- Need to sleep to allow for keys to be released for scrot
                ((super, xK_p), spawn "sleep 0.1; scrot -s"),
                ((super, xK_g), spawn "~/scripts/apps.sh"),
                ((super .|. shiftMask, xK_g), spawn "rofi -show run"),
                ((super, xK_t), spawn "~/scripts/web-search.sh"),
                ((super, xK_d), spawn "~/scripts/flatmarks.sh"),
                ((super .|. shiftMask, xK_d), spawn "~/scripts/flatmarks-work.sh"),
                ((super, xK_x), spawn "~/scripts/passmenu.sh"),
                ((super, xK_n), spawn "~/scripts/pass-prefixed-line.sh \"username: \" username"),
                ((super, xK_m), spawn "~/scripts/pass-prefixed-line.sh \"email: \" email")
              ]
                <> fmap (wsView super) ws
                <> fmap (wsSwitch super) ws
        }
