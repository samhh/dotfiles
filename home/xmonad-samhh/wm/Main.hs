{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main (main) where

import           App                         (apps)
import           Color                       (HexColor,
                                              Palette (color0, color3),
                                              getColorOrHideous, getPalette)
import qualified Data.Map                    as M
import qualified Key                         as K
import           Layout                      (layout, resetLayout)
import           Spawn                       (Spawn (..), toSpawnable)
import           StatusBar                   (statusBar)
import           Window                      (OnFullscreenDestroy (Exit),
                                              centreRect, disableFloat',
                                              enableFloat',
                                              getFullscreenEventHook,
                                              toggleFloat, toggleFullscreen',
                                              videoRect)
import           Workspace                   (ensureSpaceWindow,
                                              workspaceAutoAssign,
                                              workspaceSwap, workspaceSwitch,
                                              workspaceView)
import qualified Workspaces
import           XMonad                      (ChangeLayout (NextLayout),
                                              IncMasterN (IncMasterN),
                                              Resize (Expand, Shrink), X,
                                              XConfig (XConfig, borderWidth, clickJustFocuses, focusFollowsMouse, focusedBorderColor, handleEventHook, keys, layoutHook, manageHook, modMask, normalBorderColor, terminal, workspaces),
                                              kill, launch, restart,
                                              sendMessage, spawn, windows,
                                              withFocused, (.|.))
import           XMonad.Actions.CopyWindow   (copyToAll, killAllOtherCopies)
import           XMonad.Config.Desktop       (desktopConfig)
import           XMonad.Hooks.InsertPosition (Focus (..), Position (..),
                                              insertPosition)
import           XMonad.Hooks.ManageDocks    (docks)
import           XMonad.Layout.ResizableTile (MirrorResize (MirrorExpand, MirrorShrink))
import qualified XMonad.StackSet             as W

main :: IO ()
main = launch . docks =<< statusBar . config =<< getPalette

appName :: String
appName = "xmonad-samhh-wm"

spawn' :: MonadIO m => Spawn -> m ()
spawn' = spawn . toSpawnable

ensureSpaceBrowser :: X ()
ensureSpaceBrowser = ensureSpaceWindow "qutebrowser"

config cs = desktopConfig
  { terminal = "alacritty"
  , modMask = K.modMask
  , focusFollowsMouse = False
  , clickJustFocuses = False
  , manageHook = insertPosition Below Newer <> foldMap workspaceAutoAssign apps
  , handleEventHook = getFullscreenEventHook Exit
  , workspaces = Workspaces.name <$> Workspaces.workspaces
  , borderWidth = 3
  , normalBorderColor = c color0
  , focusedBorderColor = c color3
  , layoutHook = layout
  , keys = \cfg@XConfig {XMonad.modMask = super, XMonad.terminal = term} ->
      M.fromList $
        [ ((super, K.xK_Return), spawn term)
        , ((super .|. K.shiftMask, K.xK_q), kill)
        , ((super, K.xK_Down), windows W.focusDown)
        , ((super, K.xK_Up), windows W.focusUp)
        , ((super .|. K.shiftMask, K.xK_Down), windows W.swapDown)
        , ((super .|. K.shiftMask, K.xK_Up), windows W.swapUp)
        , ((super .|. K.shiftMask, K.xK_m), windows W.swapMaster)
        , ((super, K.xK_Left), sendMessage MirrorShrink <> sendMessage MirrorShrink)
        , ((super, K.xK_Right), sendMessage MirrorExpand <> sendMessage MirrorExpand)
        , ((super .|. K.shiftMask, K.xK_Left), sendMessage Shrink)
        , ((super .|. K.shiftMask, K.xK_Right), sendMessage Expand)
        , ((super, K.xK_r), resetLayout cfg)
        , ((super .|. K.shiftMask, K.xK_r), restart appName True)
        , ((super, K.xK_v), sendMessage NextLayout)
        , ((super, K.xK_f), toggleFullscreen')
        , ((super, K.xK_q), sendMessage . IncMasterN $ (-1))
        , ((super, K.xK_e), sendMessage . IncMasterN $ 1)
        , ((super, K.xK_s), withFocused . toggleFloat $ centreRect)
        , ((super, K.xK_a), windows copyToAll <> withFocused (enableFloat' videoRect))
        , ((super .|. K.shiftMask, K.xK_a), killAllOtherCopies <> withFocused disableFloat')
        , ((super, K.xK_o), spawn' CloseNotif)
        , ((super .|. K.shiftMask, K.xK_o), spawn' CloseAllNotifs)
        , ((K.nomod, K.xK_VolDown), spawn' DecVol)
        , ((K.nomod, K.xK_VolUp), spawn' IncVol)
        , ((super, K.xK_VolDown), spawn' DecVolMpv)
        , ((super, K.xK_VolUp), spawn' IncVolMpv)
        , ((K.nomod, K.xK_ToggleMute), spawn' ToggleMuteOutput)
        , ((super, K.xK_ToggleMute), spawn' ToggleMuteInput)
        , ((K.nomod, K.xK_MediaPrev), spawn' PlayPrevMpd)
        , ((K.nomod, K.xK_MediaTogglePlay), spawn' PauseMpd)
        , ((super, K.xK_MediaTogglePlay), spawn' PauseMpv)
        , ((K.nomod, K.xK_MediaNext), spawn' PlayNextMpd)
        , ((super, K.xK_w), spawn' NewWallpaper)
        , ((super, K.xK_p), spawn' TakeScreenshot)
        , ((super, K.xK_g), spawn' Apps)
        , ((super .|. K.shiftMask, K.xK_g), spawn' AllApps)
        , ((super, K.xK_t), ensureSpaceBrowser *> spawn' WebSearch)
        , ((super, K.xK_d), ensureSpaceBrowser *> spawn' Bookmarks)
        , ((super .|. K.shiftMask, K.xK_d), spawn' WorkBookmarks)
        , ((super, K.xK_x), spawn' Passwords)
        , ((super, K.xK_n), spawn' Usernames)
        , ((super, K.xK_m), spawn' Emails)
        , ((super, K.xK_i), spawn' LatencyCheck)
        , ((super, K.xK_z), spawn' DefinitionLookup)
        ]
          <> (workspaceView super <$> Workspaces.workspaces)
          <> (workspaceSwitch super <$> Workspaces.workspaces)
          <> (workspaceSwap super <$> Workspaces.workspaces)
  }
  where c :: (Palette -> HexColor) -> HexColor
        c = getColorOrHideous cs
