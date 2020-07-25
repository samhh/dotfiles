import qualified Data.Map as M
import System.Exit (exitSuccess)
import XMonad
import XMonad.Actions.CopyWindow (copyToAll, killAllOtherCopies)
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.DynamicLog (xmobar)
import XMonad.Hooks.InsertPosition (Focus (..), Position (..), insertPosition)
import XMonad.Hooks.ManageDocks (ToggleStruts (ToggleStruts), avoidStruts, docks)
import XMonad.Layout.MultiToggle (Toggle (Toggle), mkToggle, single)
import XMonad.Layout.MultiToggle.Instances (StdTransformers (FULL))
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Reflect (reflectHoriz)
import qualified XMonad.StackSet as W

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
videoRect = W.RationalRect (3 / 4) (3 / 4) (1 / 5) (1 / 5)

-- TODO improve this
-- from: https://www.reddit.com/r/xmonad/comments/hm2tg0/how_to_toggle_floating_state_on_a_window/fx41xx4/
toggleFloat :: Window -> X ()
toggleFloat w =
  windows
    ( \s ->
        if M.member w (W.floating s)
          then W.sink w s
          else W.float w centreRect s
    )

layout = avoidStruts $ smartBorders $ mkToggle (single FULL) $ tiled ||| reflectHoriz tiled
  where
    tiled = Tall numMaster resizeDelta masterRatio
    numMaster = 1
    resizeDelta = 3 / 100
    masterRatio = 1 / 2

main :: IO ()
main =
  (xmonad . docks)
    =<< xmobar
      desktopConfig
        { terminal = "alacritty",
          modMask = mod4Mask,
          focusFollowsMouse = False,
          clickJustFocuses = False,
          manageHook = insertPosition Below Newer,
          workspaces = fmap wsName ws,
          borderWidth = 3,
          normalBorderColor = nord0,
          focusedBorderColor = nord3,
          layoutHook = layout,
          keys = \cfg@XConfig {XMonad.modMask = super} ->
            M.fromList $
              [ ((super, xK_Return), spawn $ XMonad.terminal cfg),
                ((super .|. shiftMask, xK_q), kill),
                ((super .|. shiftMask, xK_r), io exitSuccess),
                ((super, xK_j), windows W.focusDown),
                ((super, xK_k), windows W.focusUp),
                ((super, xK_m), windows W.focusMaster),
                ((super .|. shiftMask, xK_j), windows W.swapDown),
                ((super .|. shiftMask, xK_k), windows W.swapUp),
                ((super .|. shiftMask, xK_m), windows W.swapMaster),
                ((super, xK_r), setLayout $ XMonad.layoutHook cfg),
                ((super, xK_s), sendMessage NextLayout),
                ((super, xK_f), sendMessage (Toggle FULL) <> sendMessage ToggleStruts),
                ((super, xK_q), sendMessage $ IncMasterN (-1)),
                ((super, xK_e), sendMessage $ IncMasterN 1),
                ((super, xK_s), withFocused toggleFloat),
                ((super, xK_a), windows copyToAll), -- TODO enable float here with videoRect
                ((super .|. shiftMask, xK_a), killAllOtherCopies), -- TODO disable float here
                ((nomod, xK_VolDown), spawn "amixer -Mq sset Master 1%-"),
                ((nomod, xK_VolUp), spawn "amixer -Mq sset Master 1%+"),
                ((nomod, xK_ToggleMute), spawn "amixer -q set Master toggle"),
                ((nomod, xK_MediaPrev), spawn "playerctl previous"),
                ((nomod, xK_MediaTogglePlay), spawn "playerctl play-pause"),
                ((nomod, xK_MediaNext), spawn "playerctl next"),
                ((super, xK_g), spawn "rofi -show run"),
                ((super, xK_t), spawn "~/scripts/web-search.sh"),
                ((super, xK_d), spawn "~/scripts/flatmarks.sh"),
                ((super, xK_x), spawn "~/scripts/passmenu.sh"),
                ((super, xK_n), spawn "~/scripts/pass-prefixed-line.sh \"username: \" username"),
                ((super, xK_m), spawn "~/scripts/pass-prefixed-line.sh \"email: \" email")
              ]
                <> fmap (wsView super) ws
                <> fmap (wsSwitch super) ws
        }
