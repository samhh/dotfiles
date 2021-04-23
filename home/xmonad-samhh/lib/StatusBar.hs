module StatusBar (statusBar) where

import           Data.Default                 (def)
import           String                       (trim)
import           Workspace                    (spacesWithNonCopiedWindows)
import           XMonad                       (XConfig (layoutHook, logHook))
import           XMonad.Config.Prime          (LayoutClass, Window)
import           XMonad.Hooks.DynamicLog      (PP (ppHidden, ppOrder, ppOutput, ppSep, ppTitle),
                                               dynamicLogWithPP)
import           XMonad.Hooks.ManageDocks     (AvoidStruts, avoidStruts, docks)
import           XMonad.Layout.LayoutModifier (ModifiedLayout)
import           XMonad.Util.Run              (hPutStrLn, spawnPipe)

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

barCmd :: String
barCmd = "xmobar-samhh"

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
            spaces <- spacesWithNonCopiedWindows
            let renderHidden ws | ws `elem` spaces = ws
                                | otherwise        = mempty
            logHook cfg
            dynamicLogWithPP pp {ppOutput = hPutStrLn h, ppHidden = renderHidden}
        }

