{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main (main) where

import           App                         (apps)
import           Color                       (Palette (color0, color3),
                                              getColorOrHideous, getTheme)
import           Data.Default                (def)
import qualified Data.Map                    as M
import           Function                    (bindM2)
import qualified Key                         as K
import           Layout                      (layout, resetLayout)
import           Spawn                       (BrowserProfile (Personal, Work),
                                              Spawn (..), profileInstanceName,
                                              toSpawnable)
import           StatusBar                   (statusBar)
import           Window                      (OnFullscreenDestroy (Exit),
                                              centreRect, disableFloat',
                                              enableFloat',
                                              getFullscreenEventHook,
                                              toggleFloat, toggleFullscreen',
                                              videoRect)
import           Workspace                   (spaceContainsWindow,
                                              workspaceAutoAssign,
                                              workspaceSwap, workspaceSwitch,
                                              workspaceView)
import qualified Workspaces
import           XMonad                      (ChangeLayout (NextLayout),
                                              IncMasterN (IncMasterN), Query,
                                              Resize (Expand, Shrink), Window,
                                              X,
                                              XConfig (XConfig, borderWidth, clickJustFocuses, focusFollowsMouse, focusedBorderColor, handleEventHook, keys, layoutHook, manageHook, modMask, normalBorderColor, terminal, workspaces),
                                              getDirectories, kill, launch,
                                              restart, sendMessage, spawn,
                                              windows, withFocused, (.|.), (=?))
import qualified XMonad
import           XMonad.Actions.CopyWindow   (copyToAll, killAllOtherCopies)
import           XMonad.Actions.EasyMotion   (ChordKeys (AnyKeys), sKeys,
                                              selectWindow)
import           XMonad.Config.Desktop       (desktopConfig)
import           XMonad.Hooks.InsertPosition (Focus (..), Position (..),
                                              insertPosition)
import           XMonad.Hooks.ManageDocks    (docks)
import           XMonad.Layout.ResizableTile (MirrorResize (MirrorExpand, MirrorShrink))
import           XMonad.Operations           (killWindow)
import qualified XMonad.StackSet             as W

main :: IO ()
main = bindM2 launch getCfg getDirectories
  where getCfg = fmap docks . statusBar . config =<< getTheme

instanceName :: Query String
instanceName = XMonad.appName

appName :: String
appName = "xmonad-samhh-wm"

spawn' :: MonadIO m => Spawn -> m ()
spawn' = spawn . toSpawnable

spaceHasBrowser :: BrowserProfile -> X Bool
spaceHasBrowser = spaceContainsWindow . (instanceName =?) . profileInstanceName

browserTarget :: BrowserProfile -> X String
browserTarget x = spaceHasBrowser x <&> \case
  True  -> "tab"
  False -> "window"

spawnWithBrowserTarget :: BrowserProfile -> Spawn -> X ()
spawnWithBrowserTarget x y = spawn . (toSpawnable y <>) . (" " <>) =<< browserTarget x

selectWindow' :: X (Maybe Window)
selectWindow' = selectWindow $ def { sKeys = AnyKeys colemakHomeKeys }
  where colemakHomeKeys = [K.xK_t, K.xK_n, K.xK_s, K.xK_e, K.xK_r, K.xK_i]

onSelectWindow :: (Window -> X ()) -> X ()
onSelectWindow f = (`whenJust` f) =<< selectWindow'

config t = desktopConfig
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
        , ((super .|. K.shiftMask, K.xK_q), onSelectWindow killWindow)
        , ((super .|. K.shiftMask .|. K.controlMask, K.xK_q), kill)
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
        , ((super, K.xK_h), onSelectWindow (windows . W.focusWindow))
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
        , ((super, K.xK_t), spawnWithBrowserTarget Personal WebSearch)
        , ((super, K.xK_d), spawnWithBrowserTarget Personal Bookmarks)
        , ((super .|. K.shiftMask, K.xK_d), spawnWithBrowserTarget Work WorkBookmarks)
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
  where c = getColorOrHideous t
