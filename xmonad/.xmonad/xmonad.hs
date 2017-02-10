import XMonad
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.WorkspaceNames
import XMonad.Actions.GroupNavigation
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Util.EZConfig ( additionalKeys )
import qualified Data.Map

myWorkspaces = map show [1 .. 9 :: Int]

myLayout = tiled ||| full
  where nmaster = 1 -- Default num of windows in master pane
        ratio = 1/2 -- Default ratio of master pane
        delta = 3/100 -- Percent of screen to increment when resizing
        spacingNum = 10
        full = noBorders Full
        tiled = smartBorders $ minimize $ spacing spacingNum $ gaps  [(U, 33), (D, 10), (L, 10), (R, 10)] $ ResizableTall nmaster delta ratio []

myKeys conf@(XConfig {modMask = modm}) = Data.Map.fromList $
  [ ((modm               , xK_p), spawn "exe=`rofi -show 'combi' -modi combi`")
  , ((modm .|. shiftMask , xK_p), spawn "dmenu_run")
  , ((modm               , xK_a), sendMessage MirrorShrink)
  , ((modm               , xK_z), sendMessage MirrorExpand)
  ]

myLogHook pipe = (workspaceNamesPP xmobarPP { ppOutput = hPutStrLn pipe } >>= dynamicLogWithPP)
  >> historyHook
  >> ewmhDesktopsLogHook

myConfig pipe = defaultConfig
  { terminal = "urxvt"
  , modMask = mod4Mask -- Use Super instead of Alt
  , keys =  \c -> myKeys c `Data.Map.union` keys defaultConfig c
  , borderWidth = 2
  , normalBorderColor = "#202020"
  , focusedBorderColor = "#ffa000"
  , layoutHook = myLayout
  , logHook = myLogHook pipe
  , workspaces = myWorkspaces
  }

main = xmonad . myConfig =<< spawnPipe "xmobar ~/.xmonad/xmobar.config"
