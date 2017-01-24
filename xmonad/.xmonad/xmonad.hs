import XMonad
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import XMonad.Actions.WorkspaceNames
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.Minimize

myLayout = tiled ||| Full
  where nmaster = 1 -- Default num of windows in master pane
        ratio = 1/2 -- Default ratio of master pane
        delta = 3/100 -- Percent of screen to increment when resizing
        spacingNum = 10
        tiled = minimize $ spacing spacingNum $ gaps  [(U, 33), (D, 10), (L, 10), (R, 10)] $ Tall nmaster delta ratio

myLogHook pipe = workspaceNamesPP xmobarPP { ppOutput = hPutStrLn pipe } >>= dynamicLogWithPP

myConfig pipe = defaultConfig
  { modMask = mod4Mask -- Use Super instead of Alt
  , borderWidth = 0
  , normalBorderColor = "#00000"
  , focusedBorderColor = "#d8cb63"
  , layoutHook = myLayout
  , logHook = myLogHook pipe
  }

main = xmonad . myConfig =<< spawnPipe "xmobar ~/.xmonad/xmobar.config"
