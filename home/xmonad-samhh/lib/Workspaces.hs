module Workspaces where

import qualified Graphics.X11 as XK

data WorkspaceId
  = W1
  | W2
  | W3
  | W4
  | W5
  | W6
  | W7
  | W8
  | W9
  | W0
  deriving Enum

workspaces :: [WorkspaceId]
workspaces = [toEnum 0 ..]

name :: WorkspaceId -> String
name W1 = "1"
name W2 = "2"
name W3 = "3"
name W4 = "4"
name W5 = "5"
name W6 = "6"
name W7 = "7"
name W8 = "8"
name W9 = "9"
name W0 = "0"

key :: WorkspaceId -> XK.KeySym
key W1 = XK.xK_1
key W2 = XK.xK_2
key W3 = XK.xK_3
key W4 = XK.xK_4
key W5 = XK.xK_5
key W6 = XK.xK_6
key W7 = XK.xK_7
key W8 = XK.xK_8
key W9 = XK.xK_9
key W0 = XK.xK_0

