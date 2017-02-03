import XMonad
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import XMonad.Actions.WorkspaceNames
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders

myLayout = tiled ||| full 
  where nmaster = 1 -- Default num of windows in master pane
        ratio = 1/2 -- Default ratio of master pane
        delta = 3/100 -- Percent of screen to increment when resizing
        spacingNum = 10
        full = noBorders Full
        tiled = smartBorders $ minimize $ spacing spacingNum $ gaps  [(U, 33), (D, 10), (L, 10), (R, 10)] $ Tall nmaster delta ratio

myLogHook pipe = workspaceNamesPP xmobarPP { ppOutput = hPutStrLn pipe } >>= dynamicLogWithPP

myConfig pipe = defaultConfig
  { modMask = mod4Mask -- Use Super instead of Alt
  , borderWidth = 2
  , normalBorderColor = "#00000"
  , focusedBorderColor = "#ffa000"
  , layoutHook = myLayout
  , logHook = myLogHook pipe
  }

main = xmonad . myConfig =<< spawnPipe "xmobar ~/.xmonad/xmobar.config"
