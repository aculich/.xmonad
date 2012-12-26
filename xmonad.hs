import XMonad                          hiding ((|||))
import XMonad.Actions.Commands                (screenCommands, workspaceCommands)
import XMonad.Actions.CycleWS                 (nextScreen)
import XMonad.Actions.TopicSpace
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowBringer           (bringMenu, gotoMenu)
import XMonad.Config.Gnome                    (gnomeConfig)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers             (doRectFloat, currentWs)
import XMonad.Hooks.WorkspaceByPos
import XMonad.Layout.Fullscreen
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.Tabbed
import XMonad.Layout.LayoutCombinators        ((|||), (****||*), JumpToLayout(..))
import XMonad.Layout.Magnifier
import XMonad.Layout.Master                   (mastered)
import XMonad.Layout.Maximize                 (maximize, maximizeRestore)
import XMonad.Layout.NoBorders                (smartBorders)
import XMonad.Layout.PerWorkspace             (onWorkspace)
import XMonad.Layout.Tabbed                   (defaultTheme, shrinkText, tabbedBottomAlways)
import XMonad.Layout.TrackFloating            (trackFloating)
import XMonad.Layout.WorkspaceDir
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise               (runOrRaisePrompt)
import XMonad.Prompt.Ssh                      (sshPrompt)
import XMonad.Prompt.Workspace                (workspacePrompt)
import XMonad.Prompt.XMonad                   (xmonadPromptC)
import XMonad.StackSet hiding                 (workspaces)
import XMonad.Util.Replace                    (replace)

import Graphics.X11.Xlib
import qualified Data.Map as M

import System.Exit

import Data.Ratio                             ((%))
import Data.Maybe
import Control.Applicative                    ((<$>))
import Control.Monad.Error                    ((<=<),guard,lift,runErrorT,throwError)


myTopics :: [Topic]
myTopics =
  [ "emacs"
  , "shell"
  , "web"
  , "pidgin"

  , "mail"
  , "web2"
  , "reader"
  , "music"

  , "misc1"
  , "misc2"
  , "misc3"
  , "misc4"

  , "affine"
  , "affineweb"
  , "affinemail"
  ]

myTopicConfig :: TopicConfig
myTopicConfig = defaultTopicConfig
  { topicDirs = M.fromList $
      [ ("emacs"     ,  "")
      , ("shell"     ,  "")
      , ("web"       ,  "")
      , ("pidgin"    ,  "")

      , ("mail"      ,  "")
      , ("web2"      ,  "")
      , ("reader"    ,  "")
      , ("music"     ,  "")

      , ("misc1"     ,  "")
      , ("misc2"     ,  "")
      , ("misc3"     ,  "")
      , ("misc4"     ,  "")
      ]
  , defaultTopicAction = const $ spawnShell >*> 3
  , defaultTopic = "emacs"
  , topicActions = M.fromList $
      [ ("emacs"     , spawn "emacs")
      , ("shell"     , spawn "x-terminal-emulator")
      , ("web"       , spawn "x-terminal-emulator")
      , ("pidgin"    , spawn "pidgin")

      , ("mail"      , spawn "chromium-browser --app=http://mail.google.com/")
      , ("web2"      , spawn "chromium-browser")
      , ("reader"    , spawn "chromium-browser --app=http://reader.google.com/")
      , ("music"     , spawn "chromium-browser --user-data-dir=~/.config/profiles/music --app=http://last.fm")

      , ("misc1"     , spawn "x-terminal-emulator")
      , ("misc2"     , spawn "x-terminal-emulator")
      , ("misc3"     , spawn "x-terminal-emulator")
      , ("misc4"     , spawn "x-terminal-emulator")
      ]
  }

myXPConfig :: XPConfig
myXPConfig = defaultXPConfig {font="-*-lucida-medium-r-*-*-14-*-*-*-*-*-*-*", height=22}

spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

spawnShellIn :: Dir -> X ()
spawnShellIn dir = spawn $ "x-terminal-emulator --working-directory=" ++ dir

goto :: Topic -> X ()
goto = switchTopic myTopicConfig

promptedGoto :: X ()
promptedGoto = workspacePrompt myXPConfig goto

promptedShift :: X ()
promptedShift = workspacePrompt myXPConfig $ windows . shift

customCommands :: X [(String, X ())]
customCommands = do
    wscmds <- workspaceCommands
    return $ otherCommands ++ screenCommands ++ wscmds
 where
    defaultLayout = asks (layoutHook . config) >>= setLayout
    quitWM        = io $ exitWith ExitSuccess
    run           = spawn "exe=`dmenu_path | dmenu -b` && exec $exe"
    sinkFocused   = withFocused $ windows . sink
    xterm         = spawn =<< asks (terminal .  config)
    otherCommands =
        [ ("shrink"              , sendMessage Shrink       )
        , ("expand"              , sendMessage Expand       )

        , ("spawn"               , spawnShell               )
        , ("spawn2"              , spawnShell >*> 2         )
        , ("spawn3"              , spawnShell >*> 3         )
        , ("spawnin"             , spawnShellIn "/projects" )

        , ("next-layout"         , sendMessage NextLayout   )
        , ("default-layout"      , defaultLayout            )
        , ("restart-wm"          , restart "xmonad" True    )
        , ("restart-wm-no-resume", restart "xmonad" False   )
        , ("xterm"               , xterm                    )
        , ("run"                 , run                      )
        , ("kill"                , kill                     )
        , ("refresh"             , refresh                  )
        , ("focus-up"            , windows focusUp          )
        , ("focus-down"          , windows focusDown        )
        , ("swap-up"             , windows swapUp           )
        , ("swap-down"           , windows swapDown         )
        , ("swap-master"         , windows swapMaster       )
        , ("sink-focused"        , sinkFocused              )
        , ("quit-wm"             , quitWM                   )
        ]

customPrompt :: XPConfig -> X ()
customPrompt c = do
    cmds <- customCommands
    xmonadPromptC cmds c

altMask  = mod1Mask
metaMask = mod4Mask
asMask   = altMask     .|. shiftMask
casMask  = controlMask .|. shiftMask .|. mod1Mask
casmMask = controlMask .|. shiftMask .|. mod1Mask .|. metaMask
csMask   = controlMask .|. shiftMask
caMask   = controlMask .|. altMask
cmMask   = controlMask .|. metaMask
cmsMask  = controlMask .|. metaMask  .|. shiftMask

-- my default modifier key is a whole chord which may be awkward at
-- first, but isn't as difficult to get used to as you might think:

--    Control-Shift-Alt

-- The main intent is to avoid clashing with any program keybindings;
-- this is a pretty safe choice that I've never experienced a conflict
-- with. It makes the assumption that the Control key has been moved
-- to its rightful place, which is to the left of the A key replacing
-- the accursed capslock key. There is a variant of this chord which
-- adds Meta that I press with my thumb at the same time as the Alt
-- key:

--  Control-Shift-Meta-Alt

-- Suspend your disbelief and give it a try for a while. Then give me
-- some feedback after using it for a week.

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    -- global
    ((modMask,      xK_q            ), spawn "xmonad --recompile && xmonad --restart")
  , ((modMask,      xK_c            ), kill)

    -- change directory for workspace
  , ((modMask,      xK_v            ), changeDir defaultXPConfig)
  , ((casmMask,     xK_n            ), customPrompt defaultXPConfig)

    -- layouts
  , ((casmMask,     xK_space        ), sendMessage NextLayout) -- %! Rotate through the available layout algorithms
  , ((modMask,      xK_f            ), sendMessage ToggleStruts)

    -- screens
  , ((casmMask,     xK_f            ), sendMessage $ JumpToLayout "Full")  -- jump directly to the Full layout
  , ((modMask,      xK_h            ), nextScreen)

  , ((casMask,      xK_Return       ), runOrRaisePrompt defaultXPConfig)
  , ((casmMask,     xK_Return       ), sshPrompt defaultXPConfig)

    -- bring or goto window
  , ((modMask,      xK_g            ), gotoMenu)
  , ((modMask,      xK_b            ), bringMenu)
  , ((casmMask,     xK_b            ), bringMenu)

    -- window size
  , ((modMask,      xK_x            ), withFocused (sendMessage . maximizeRestore))
  , ((modMask,      xK_backslash    ), withFocused (sendMessage . maximizeRestore))
  , ((modMask,      xK_equal        ), sendMessage MagnifyMore)
  , ((modMask,      xK_minus        ), sendMessage MagnifyLess)
  , ((modMask,      xK_apostrophe   ), sendMessage Toggle)

    -- window position
  , ((modMask,      xK_z            ), windows swapMaster)
  , ((modMask,      xK_a            ), windows swapUp)
  , ((modMask,      xK_d            ), windows swapDown)

    -- window focus
  , ((altMask,      xK_Tab          ), windows focusDown)
  , ((asMask,       xK_Tab          ), windows focusUp)

    -- resizing the master/slave ratio
  , ((casMask,      xK_bracketleft  ), sendMessage Shrink)  -- %! Shrink the master area
  , ((casMask,      xK_bracketright ), sendMessage Expand) -- %! Expand the master area

    -- floating layer support
  , ((modMask,      xK_t            ), withFocused $ windows . sink)  -- %! Push window back into tiling

  , ((modMask,      xK_n            ), promptedGoto)
  , ((casmMask,     xK_n            ), refresh)

  -- increase or decrease number of windows in the master area
  , ((casMask,      xK_1            ), sendMessage (IncMasterN 1))    -- %! Increment number of windows in master area
  , ((casmMask,     xK_1            ), sendMessage (IncMasterN (-1))) -- %! Deincrement number of windows in master area
  -- , ((casMask, xK_1              ), increaseLimit)
  -- , ((casmMask, xK_1             ), decreaseLimit)

  ]

  ++

  -- the below keys form a rectangular 4x3 grid that corresponds
  -- logically to the workspaces; remembering positionally is not only
  -- easier than remembering numbers 1-9 (like most people use for
  -- workspace switching), but it also allows you to stay on the home
  -- row, so switching between workspaces is very fast. There are two
  -- different chording combinations:

  -- View:  Contol-Shift-Alt <key>
  -- Shift: Control-Shift-Meta-Alt <key>

  -- Where key is one of:

  --    uipo
  --    jk;l
  --    m,./

  -- On startup the default position is j.

  [((m .|. modMask, k), windows $ f i)
  | (i, k) <- zip (XMonad.workspaces conf)
              [xK_j, xK_k, xK_l, xK_semicolon,
               xK_m, xK_comma, xK_period, xK_slash,
               xK_u, xK_i, xK_o, xK_p]
  , (f, m) <- [(myView, 0), (shift, casmMask)]]

  ++
  -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
  | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
  , (f, m) <- [(myView, 0), (shift, shiftMask)]]

myView = greedyView
myLayoutModifiers l = workspaceDir "~"
                      $ avoidStruts
                      $ smartBorders
                      $ maximize
                      $ trackFloating
                      $ fullscreenFull
                      $ magnifierOff
                      $ onWorkspace "pidgin" pidginLayout
                      $ l
  where
     pidginLayout   = withIM (1/8) (Role "buddy_list") Grid

myLayout = myLayoutModifiers (masteredLayout ||| Full ||| gimpLayout)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled          = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster        = 1

     -- Default proportion of screen occupied by master pane
     ratio          = 1/2

     -- Percent of screen to increment by when resizing panes
     delta          = 3/100

     masteredLayout = (mastered (1/100) (2/3) $ Grid ||| Full)

     tabbedLayout   = tabbedBottomAlways shrinkText defaultTheme
     gimpLayout     = tabbedLayout ****||* Full


myEventHook = fullscreenEventHook

myLogHook = updatePointer (Relative 0.8 0.6) >> dynamicLog

myFloats :: ManageHook
myFloats = composeAll
           [ className =? "MPlayer"           --> doFloat
           , className =? "feh"               --> doFloat
           , className =? "Xephyr"            --> doFloat
           , className =? "Unity-2d-panel"    --> doIgnore
           , className =? "Unity-2d-launcher" --> doFloat
           ]

doSink :: ManageHook
doSink = ask >>= doF . sink

mySinks :: ManageHook
mySinks = composeAll
          [ className  =? "Tsclient"          --> doSink
          , className  =? "rdesktop"          --> doSink
          , className  =? "image"             --> doSink
          , className  =? "Image"             --> doSink
          , className  =? "Apport-gtk"        --> doSink
          , className  =? "Pidgin"            --> doSink
          ]

myShifts :: ManageHook
myShifts = composeAll
           [ className =? "Pidgin"            --> doShift "pidgin" ]

insertAfterMaster = insertPosition Below Newer
myManageHook :: ManageHook
myManageHook = fullscreenManageHook <+>
               insertAfterMaster <+>
               myShifts <+>
               mySinks <+>
               myFloats <+>
               workspaceByPos <+>
               manageHook gnomeConfig

myConfig = gnomeConfig {
  layoutHook   = myLayout
  , modMask    = casMask
  , keys       = myKeys
  , logHook    = myLogHook
  , workspaces = myTopics
  , manageHook = myManageHook
  , terminal   = "x-terminal-emulator"
  }

main = do
  checkTopicConfig myTopics myTopicConfig
  replace
  xmonad =<< xmobar myConfig
