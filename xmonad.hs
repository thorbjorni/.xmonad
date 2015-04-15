import Control.Monad
import Control.Monad.Trans
import Data.Bits ((.|.))
import Data.Map (fromList)
import Data.Monoid
import Data.Ratio
import GHC.Real
import System.Exit
--import XMonad
import XMonad
import XMonad.StackSet hiding (workspaces)
import XMonad.Actions.SpawnOn
import XMonad.Actions.Warp
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.Grid
import XMonad.Layout.IndependentScreens
import XMonad.Layout.Magnifier
import XMonad.Layout.NoBorders
import XMonad.Util.Dzen
import XMonad.Util.EZConfig
import XMonad.Util.Run
 
centerMouse = warpToWindow (1/2) (1/2)
statusBarMouse = warpToScreen 0 (5/1600) (5/1200)
withScreen screen f = screenWorkspace screen >>= flip whenJust (windows . f)
 
makeLauncher yargs run exec close = concat
    ["exe=`yeganesh ", yargs, "` && ", run, " ", exec, "$exe", close]
launcher     = makeLauncher "" "eval" "\"exec " "\""
termLauncher = makeLauncher "-p withterm" "exec urxvt -e" "" ""
viewShift  i = view i . shift i
floatAll     = composeAll . map (\s -> className =? s --> doFloat)
sinkFocus    = peek >>= maybe id sink

 
bright = "#80c0ff"
dark   = "#13294e"
redcolor = "#3279a8"
 
fullscreenMPlayer = className =? "MPlayer" --> do
    dpy   <- liftX $ asks display
    win   <- ask
    hints <- liftIO $ getWMNormalHints dpy win
    case fmap (approx . fst) (sh_aspect hints) of
        Just ( 4 :% 3)  -> viewFullOn win 0 "1"
        Just (16 :% 9)  -> viewFullOn win 1 "5"
        _               -> doFloat
    where
    fi               = fromIntegral
    approx (n, d)    = approxRational (fi n / fi d) (1/100)
    viewFullOn w s n = do
        let ws = marshall s n
        liftX  $ withScreen s view
        return . Endo $ view ws . shiftWin ws w
 
main = do
    nScreens    <- countScreens
    hs          <- mapM (spawnPipe . xmobarCommand) [0 .. nScreens-1]
    xmonad $ defaultConfig {
        borderWidth             = 1,
        workspaces              = withScreens nScreens (map show [1..5]),
        terminal                = "urxvt",
        normalBorderColor       = dark,
        focusedBorderColor      = bright,
        modMask                 = mod4Mask,
        keys                    = keyBindings,
        layoutHook              = magnifierOff $ avoidStruts (GridRatio 0.9) ||| noBorders Full,
        manageHook              = floatAll ["Gimp", "Wine"]
                                  <+> (title =? "CGoban: Main Window" --> doF sinkFocus)
                                  <+> (isFullscreen --> doFullFloat)
                                  <+> fullscreenMPlayer
                                  <+> manageDocks
                                  <+> manageSpawn,
        logHook                 = mapM_ dynamicLogWithPP $ zipWith pp hs [0..nScreens],
        startupHook             = setWMName "LG3D"
        }
 
keyBindings conf = let m = modMask conf in fromList $ [
    ((m .|. shiftMask,      xK_Return  ), spawnHere "urxvt"),
    ((m                , xK_p          ), spawn "gmrun"),
    ((m .|. shiftMask  , xK_c          ), kill),
    ((m                , xK_q          ), restart "xmonad" True),
    ((m .|. shiftMask  , xK_q          ), io (exitWith ExitSuccess)),
    ((m                , xK_space      ), sendMessage NextLayout),
    ((m .|. shiftMask  , xK_space      ), setLayout $ layoutHook conf),
    ((m                , xK_o          ), sendMessage Toggle),
    ((m                , xK_x          ), withFocused (windows . sink)),
    ((m                , xK_k          ), windows focusUp),
    ((m .|. shiftMask  , xK_k          ), windows swapUp),
    ((m                , xK_Tab        ), spawn "conkytoggle.sh"),
    ((m                , xK_j          ), windows focusDown),
    ((m .|. shiftMask  , xK_j          ), windows swapDown),
    ((m                , xK_a          ), windows focusMaster),
    ((m .|. shiftMask  , xK_a          ), windows swapMaster),
    ((m                , xK_w  ), withScreen 0 view),
    ((m .|. shiftMask  , xK_Control_L  ), withScreen 0 viewShift),
    ((0,                          0x1008ff12  ), spawn "dvol -t & volpipe.sh"),        -- XF86AudioMute
    ((0,                          0x1008ff11  ), spawn "dvol -d 2 & volpipe.sh"),   -- XF86AudioLowerVolume
    ((0,                          0x1008ff13  ), spawn "dvol -i 2 & volpipe.sh"),   -- XF86AudioRaiseVolume
    ((0,                          0x1008ff14  ), spawn "spotify-change.sh playpause"),
    ((0,                          0x1008ff17  ), spawn "spotify-change.sh next"),
    ((0,                          0x1008ff16  ), spawn "spotify-change.sh previous"),
    ((m                , xK_e      ), withScreen 1 view),
    ((m .|. shiftMask  , xK_Alt_L      ), withScreen 1 viewShift),
    ((m                , xK_u          ), centerMouse),
    ((m .|. controlMask, xK_u          ), centerMouse),
    ((m .|. mod1Mask   , xK_u          ), centerMouse),
    --((m                ,xK_h           ), sendMessage Shrink),                          -- %! Shrink a master area
    --((m                ,xK_l           ), sendMessage Expand),                          -- %! Expand a master area
    ((m .|. shiftMask  , xK_u          ), statusBarMouse),
    ((m .|. shiftMask  , xK_s          ), spawn "slock")
    ] ++ [
    ((m .|. e .|. i    , key           ), windows (onCurrentScreen f workspace))
    | (key, workspace) <- zip [xK_1..xK_9] (workspaces' conf)
    , (e, f)           <- [(0, view), (shiftMask, viewShift)]
    , i                <- [0, controlMask, mod1Mask, controlMask .|. mod1Mask]
    ]
 
xmobarCommand (S s) = unwords ["xmobar /home/smari/.xmonad/xmobarrc", "-i", "/home/smari/Downloads", "-x", show s, "-t", template s] where
	template 0 = "::%StdinReader%::%dynnetwork%::}{::%uname2%@%uname1%::"
	template _ = "::%StdinReader%::}{::VOL:%vol_pipe%::%date%::"

pp h s = marshallPP s defaultPP {
    ppCurrent           = color "green",
    ppVisible           = color "white",
    ppHiddenNoWindows   = color dark,
    ppUrgent            = color "red",
    ppSep               = "",
    ppOrder             = \(wss:layout:title:_) -> [wss],
    ppOutput            = hPutStrLn h
    }
    where color c = xmobarColor c ""
