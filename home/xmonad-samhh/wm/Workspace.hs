module Workspace (spacesWithNonCopiedWindows, workspaces, workspaceName, workspaceView, workspaceSwitch) where

import           Data.Maybe.Utils    (singletonToMaybe)
import           Foreign.C.String    (peekCString)
import           XMonad.Config.Prime (Display, KeyMask, KeySym, Layout,
                                      TextProperty (tp_value), Window,
                                      WorkspaceId, X, XState (windowset),
                                      getTextProperty, shiftMask, wM_NAME,
                                      windows, (.|.))
import qualified XMonad.Config.Prime as XK
import           XMonad.Core         (withDisplay)
import           XMonad.StackSet     (Workspace, greedyView, hidden, integrate',
                                      shift, stack, tag)

type WorkspaceName = String

spacesWithNonCopiedWindows :: X [WorkspaceId]
spacesWithNonCopiedWindows = fmap (fmap tag) . filterOutCopies . hidden =<< gets windowset
  where filterOutCopies :: [Workspace WorkspaceId (Layout Window) Window] -> X [Workspace WorkspaceId (Layout Window) Window]
        filterOutCopies xs = do
          nonCopiedWindows <- mapMaybe singletonToMaybe . group . sort . join <$> traverse getWorkspaceWindowTitles xs
          let isAnyNonCopied = any (`elem` nonCopiedWindows)
          filterM (fmap isAnyNonCopied . getWorkspaceWindowTitles) xs

getWorkspaceWindowTitles :: Workspace i l Window -> X [String]
getWorkspaceWindowTitles w = withDisplay $ \d ->
  liftIO $ forM (integrate' $ stack w) (`getWindowTitle` d)

getWindowTitle :: Window -> Display -> IO String
getWindowTitle w d = getTextProperty d w wM_NAME >>= (peekCString . tp_value)

type NamedWorkspace = (WorkspaceName, KeySym)

workspaces :: [NamedWorkspace]
workspaces =
  [ ("1", XK.xK_1),
    ("2", XK.xK_2),
    ("3", XK.xK_3),
    ("4", XK.xK_4),
    ("5", XK.xK_5),
    ("6", XK.xK_6),
    ("7", XK.xK_7),
    ("8", XK.xK_8),
    ("9", XK.xK_9),
    ("0", XK.xK_0)
  ]

workspaceName :: NamedWorkspace -> WorkspaceName
workspaceName = fst

workspaceView :: KeyMask -> NamedWorkspace -> ((KeyMask, KeySym), X ())
workspaceView super (name, k) =
  let x = windows $ greedyView name
   in ((super, k), x)

workspaceSwitch :: KeyMask -> NamedWorkspace -> ((KeyMask, KeySym), X ())
workspaceSwitch super (name, k) =
  let x = windows $ shift name
   in ((super .|. shiftMask, k), x)


