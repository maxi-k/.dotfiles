import XMonad
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Actions.WorkspaceNames
import XMonad.Actions.GroupNavigation
import XMonad.Layout.Tabbed
import XMonad.Layout.Spacing
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Util.EZConfig ( additionalKeys )
import qualified Data.Map

mySpacing :: Int
mySpacing = 10

toggleSpacing :: Int -> Int
toggleSpacing 0 = mySpacing
toggleSpacing _ = 0 


myLayout = avoidStruts (tiled ||| full)
  where nmaster = 1 -- Default num of windows in master pane
        ratio = 1/2 -- Default ratio of master pane
        delta = 3/100 -- Percent of screen to increment when resizing
        full = noBorders Full
        tiled = smartBorders . minimize . (spacingWithEdge mySpacing) $
                ResizableTall nmaster delta ratio []

myTerminal = "urxvt"
myStatusBar = "xmobar ~/.xmonad/xmobar.config"
myToggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

  -- Custom PP, configure it as you like. It determines what is being written to the bar.
myPP = xmobarPP { ppVisible = xmobarColor "#404040" ""
                , ppCurrent = xmobarColor "#ee9a00" ""
                , ppTitle = xmobarColor "#ee9a00" ""
                  -- , ppHiddenNoWindows = xmobarColor "#222222" ""
                  -- , ppLayout = xmobarColor"#790a0a" ""
                , ppUrgent = xmobarColor "#900000" "" . wrap "[" "]"
                , ppWsSep = "  "
                , ppOrder = (\(workspaces:layouts:title:_) -> [workspaces, title])
                }

myWorkspaces = clickable . (map xmobarEscape) $
  ["1 \xf108 ","2 \xf120 ","3 \xf268 ","4 \xf0e0 ","5 \xf111 ","6 \xf111 ","7 \xf111 " ,"8 \xf111 ","9 \xf001 "]
  where clickable l = [ "<action=xdotool key super+" ++ show n ++ ">" ++ ws ++ "</action>" | (i,ws) <- zip [1..9] l,
                        let n = i ]
        xmobarEscape = concatMap doubleLts
          where doubleLts '<' = "<<"
                doubleLts x = [x]

myKeys conf@(XConfig {modMask = modm}) = Data.Map.fromList $
  [ ((modm               , xK_p), spawn "exe=`rofi -show 'combi' -modi combi`")
  , ((modm .|. shiftMask , xK_p), spawn "dmenu_run")
  , ((modm               , xK_g), sendMessage $ ModifySpacing toggleSpacing )
  , ((modm               , xK_a), sendMessage MirrorShrink)
  , ((modm               , xK_z), sendMessage MirrorExpand)
  , ((modm               , xK_m), withFocused minimizeWindow)
  , ((modm .|. shiftMask , xK_m), sendMessage RestoreNextMinimizedWin)
  ]

myConfig = def
  { terminal = myTerminal
  , modMask = mod4Mask -- Use Super instead of Alt
  , keys =  \c -> myKeys c `Data.Map.union` keys def c
  , borderWidth = 2
  , normalBorderColor = "#202020"
  , focusedBorderColor = "#ffa000"
  , layoutHook = myLayout
  -- , logHook = myLogHook pipe
  , startupHook = do
      spawn "~/.xmonad/startup-hook" 
      setWMName "LG3D"
  , workspaces = myWorkspaces
  }

main = xmonad =<< statusBar myStatusBar myPP myToggleStrutsKey myConfig
