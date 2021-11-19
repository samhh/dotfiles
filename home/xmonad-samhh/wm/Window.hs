module Window where

import           Data.Datum
import qualified Data.Map                            as M
import           Function                            (if2, ($.))
import           System.IO.Unsafe                    (unsafePerformIO)
import           XMonad.Config.Prime                 (Event (DestroyWindowEvent, ev_event, ev_window),
                                                      LayoutClass (description),
                                                      Query, Window, WindowSet,
                                                      X, XState (windowset),
                                                      className, liftX,
                                                      runQuery, sendMessage,
                                                      whenX, windows, (=?))
import           XMonad.Hooks.ManageDocks            (ToggleStruts (ToggleStruts))
import           XMonad.Layout                       (Full (Full))
import           XMonad.Layout.MultiToggle           (Toggle (Toggle))
import           XMonad.Layout.MultiToggle.Instances (StdTransformers (FULL))
import           XMonad.StackSet                     (RationalRect (RationalRect),
                                                      current, float, floating,
                                                      layout, peek, sink,
                                                      workspace)

centreRect :: RationalRect
centreRect = RationalRect (1 / 3) (1 / 3) (1 / 3) (1 / 3)

videoRect :: RationalRect
videoRect = RationalRect offset offset size size
  where
    size = 1 / 4
    offset = 1 - size - (size / 8)

isFloating :: Window -> WindowSet -> Bool
isFloating w s = M.member w (floating s)

enableFloat :: RationalRect -> Window -> (WindowSet -> WindowSet)
enableFloat = flip float

enableFloat' :: RationalRect -> Window -> X ()
enableFloat' = windows $. enableFloat

disableFloat :: Window -> (WindowSet -> WindowSet)
disableFloat = sink

disableFloat' :: Window -> X ()
disableFloat' = windows . disableFloat

toggleFloat :: RationalRect -> Window -> X ()
toggleFloat r = windows . if2 isFloating disableFloat (enableFloat r)

toggleFullscreen' :: X ()
toggleFullscreen' = sendMessage (Toggle FULL) <> sendMessage ToggleStruts

layoutName :: Query String
layoutName = liftX . gets $ description . layout . workspace . current . windowset

isFullscreenQuery :: Query Bool
isFullscreenQuery = layoutName =? show Full

-- We'll no longer have access to the window information we need by the time
-- `getFullscreenEventHook` is run on a destroy event, so this keeps track of
-- the last two focused windows' class names as a workaround. We keep track of
-- two because the latest will be too new - the window which has assumed focus
-- from that which was destroyed.
lastClassNames :: IORef (Datum String)
{-# NOINLINE lastClassNames #-}
lastClassNames = unsafePerformIO $ newIORef mempty

trackFocused :: X ()
trackFocused = foldMap updateClassNames =<< gets (peek . windowset)
  where updateClassNames = modifyIORef lastClassNames . renew <=< runQuery className

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
  when (w == evt) $
    whenX (runQuery isFullscreenQuery w &&^ (not <$> isIgnorable))
      toggleFullscreen'
  pure $ All True
    where isIgnorable = (== "pinentry-qt") . prev <$> readIORef lastClassNames
getFullscreenEventHook _ _ = pure $ All True
