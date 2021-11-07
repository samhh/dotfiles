module Workspace where

import           Data.Maybe.Utils              (singletonToMaybe)
import           Foreign.C.String              (peekCString)
import qualified Workspaces                    as WS
import           XMonad
import           XMonad.Actions.SwapWorkspaces (swapWithCurrent)
import           XMonad.StackSet               (Workspace, current, greedyView,
                                                hidden, integrate', shift,
                                                stack, tag, workspace)

spaceContainsWindow :: Query Bool -> X Bool
spaceContainsWindow p = anyM (runQuery p) . getWorkspaceWindows =<< ws
  where ws = gets $ workspace . current . windowset

spacesWithNonCopiedWindows :: X [WorkspaceId]
spacesWithNonCopiedWindows = fmap (fmap tag) . filterOutCopies . hidden =<< gets windowset
  where filterOutCopies :: [Workspace WorkspaceId (Layout Window) Window] -> X [Workspace WorkspaceId (Layout Window) Window]
        filterOutCopies xs = do
          nonCopiedWindows <- mapMaybe singletonToMaybe . group . sort . join <$> traverse getWorkspaceWindowTitles xs
          let isAnyNonCopied = any (`elem` nonCopiedWindows)
          filterM (fmap isAnyNonCopied . getWorkspaceWindowTitles) xs

getWorkspaceWindows :: Workspace i l Window -> [Window]
getWorkspaceWindows = integrate' . stack

getWorkspaceWindowTitles :: Workspace i l Window -> X [String]
getWorkspaceWindowTitles w = withDisplay $ \d ->
  liftIO $ forM (getWorkspaceWindows w) (`getWindowTitle` d)

getWindowTitle :: Window -> Display -> IO String
getWindowTitle w d = getTextProperty d w wM_NAME >>= (peekCString . tp_value)

workspaceView :: KeyMask -> WS.WorkspaceId -> ((KeyMask, KeySym), X ())
workspaceView super wid =
  let x = windows . greedyView . WS.name $ wid
   in ((super, WS.key wid), x)

workspaceSwitch :: KeyMask -> WS.WorkspaceId -> ((KeyMask, KeySym), X ())
workspaceSwitch super wid =
  let x = windows . shift . WS.name $ wid
   in ((super .|. shiftMask, WS.key wid), x)

workspaceSwap :: KeyMask -> WS.WorkspaceId -> ((KeyMask, KeySym), X ())
workspaceSwap super wid =
  let x = windows . swapWithCurrent . WS.name $ wid
   in ((super .|. shiftMask .|. controlMask, WS.key wid), x)

inSpaceElse :: Query Bool -> X () -> X ()
p `inSpaceElse` f = spaceContainsWindow p >>= \case
  True  -> pure ()
  False -> f

ensureSpaceWindow :: String -> X ()
ensureSpaceWindow x = (className =? x) `inSpaceElse` spawn x
