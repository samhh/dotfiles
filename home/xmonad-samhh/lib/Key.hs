{- HLINT ignore "Use camelCase" -}

module Key (nomod, modMask, xK_VolDown, xK_VolUp, xK_ToggleMute, xK_MediaPrev, xK_MediaTogglePlay, xK_MediaNext, module Graphics.X11.Types) where

import           Graphics.X11.Types

nomod :: KeyMask
nomod = 0

modMask :: KeyMask
modMask = mod4Mask

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


