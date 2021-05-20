{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Layout (layout, resetLayout) where

import           XMonad                              (Layout, layoutHook)
import           XMonad.Config.Prime                 (X, XConfig, setLayout,
                                                      (|||))
import           XMonad.Hooks.ManageDocks            (avoidStruts)
import           XMonad.Hooks.RefocusLast            (refocusLastLayoutHook)
import           XMonad.Layout.IfMax                 (ifMax)
import           XMonad.Layout.MultiToggle           (mkToggle, single)
import           XMonad.Layout.MultiToggle.Instances (StdTransformers (FULL))
import           XMonad.Layout.NoBorders             (smartBorders)
import           XMonad.Layout.Reflect               (reflectHoriz)
import           XMonad.Layout.ResizableTile         (ResizableTall (ResizableTall))
import           XMonad.Layout.Spacing               (Border (Border),
                                                      spacingRaw)

layout = withUniAcc $ ifMax 1 (withFixedAcc tiled) (withTiledAcc tiled)
  where
    withUniAcc = avoidStruts . smartBorders . mkToggle (single FULL)
    withFixedAcc = getGaps (Border 6 6 250 250)
    withTiledAcc = refocusLastLayoutHook . flippable . getGaps (Border 6 6 6 6)
    flippable x = x ||| reflectHoriz x
    getGaps x = spacingRaw False x True x True
    tiled = ResizableTall numMaster resizeDelta masterRatio mempty
    numMaster = 1
    resizeDelta = 3 / 100
    masterRatio = 1 / 2

resetLayout :: XConfig Layout -> X ()
resetLayout = setLayout . layoutHook

