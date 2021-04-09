module Workspace (workspaceWithOnlyFocusedCopy, workspaces, workspaceName, workspaceView, workspaceSwitch) where

import           Function            ((<$<))
import           XMonad.Config.Prime (KeyMask, KeySym, Window, WindowSpace,
                                      WorkspaceId, X, XState (windowset),
                                      shiftMask, windows, (.|.))
import qualified XMonad.Config.Prime as XK
import           XMonad.StackSet     (down, greedyView, hidden, integrate',
                                      peek, shift, stack, tag, up)

-- | A list of hidden workspaces containing a copy of the focused window and
--   nothing else.
workspaceWithOnlyFocusedCopy :: X [WorkspaceId]
workspaceWithOnlyFocusedCopy = do
    wset <- gets windowset
    pure $ case peek wset of
      Nothing -> []
      Just fw -> spacesContainingOnly fw (hidden wset)

spacesContainingOnly :: Window -> [WindowSpace] -> [WorkspaceId]
spacesContainingOnly x = tag <$< filter (uncurry (&&) . (hasWindow x &&& hasSingleWindow))
  where hasWindow :: Window -> WindowSpace -> Bool
        hasWindow w = (w `elem`) . integrate' . stack

        hasSingleWindow :: WindowSpace -> Bool
        hasSingleWindow w = case stack w of
          Nothing -> False
          Just s  -> null $ up s <> down s

type Workspace = (String, KeySym)

workspaces :: [Workspace]
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

workspaceName :: Workspace -> String
workspaceName = fst

workspaceView :: KeyMask -> Workspace -> ((KeyMask, KeySym), X ())
workspaceView super (name, k) =
  let x = windows $ greedyView name
   in ((super, k), x)

workspaceSwitch :: KeyMask -> Workspace -> ((KeyMask, KeySym), X ())
workspaceSwitch super (name, k) =
  let x = windows $ shift name
   in ((super .|. shiftMask, k), x)


