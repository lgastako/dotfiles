import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Run(spawnPipe)
import XMonad.Hooks.SetWMName
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig

import qualified XMonad.StackSet as W

import System.IO

myManageHook = composeAll . concat $
    [ [ className =? "Firefox-bin" --> doShift "web"  ],
     -- Makes YouTube and other flash videos full-screenable
      [ isFullscreen --> doFullFloat ]
    ]

myWorkspaces = [ "web", "emacs", "codeterms" ] ++ map show [4..7] ++ [ "music", "chat" ]

main = do
    conf <- dzen defaultConfig
    xmonad $ conf
        { workspaces = myWorkspaces,
          manageHook = myManageHook <+> manageDocks <+>
                       manageHook defaultConfig,
          layoutHook = avoidStruts $ smartBorders $ layoutHook defaultConfig,
          borderWidth = 1,
          startupHook = setWMName "LG3D",
          terminal = "urxvt -cd /home/john -rv -tr -sh 15 -sl 9999 -fn \"xft:Mono:size=13\"",
          logHook = dynamicLogWithPP $ dzenPP
        } `additionalKeys` myKeys

myKeys =
    [ ((mod1Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock"),
      ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s"),
      ((0, xK_Print), spawn "scrot")
    ]
    ++
    [((m .|. mod1Mask, k), windows $ f i)
         | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
         , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
