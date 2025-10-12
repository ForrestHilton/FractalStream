module UI.Menu
  ( makeMenuBar
  ) where

import FractalStream.Metadata

import UI.ProjectActions

import Graphics.UI.WX hiding (when)

-- | Make a standard menu bar for `f`, plus any additional
-- menus we are given. Apparently this should be run after
-- setting the layout of `f`?
makeMenuBar :: ProjectActions -> Frame a -> [Menu ()] -> IO ()
makeMenuBar ProjectActions{..} f addlMenus = do

  let getProject verb k = maybe (pure ()) k =<<
        fileOpenDialog objectNull True True
           (verb ++ " a FractalStream project")
           [ ("FractalStream project files", ["*.yaml"])
           , ("All files", ["*.*"])
           ] "" ""

  -- Build the project menu
  prj <- menuPane [ text := "&Project" ]
  menuItem prj [ text := "&New project"
               , help := "Create a new FractalStream project"
               , on command := projectNew ]
  menuItem prj [ text := "&Open project"
               , help := "Open an existing FractalStream project"
               , on command := getProject "Open" projectOpen ]
  menuItem prj [ text := "&Edit project"
               , help := "Modify an existing FractalStream project"
               , on command := getProject "Edit" projectEdit ]

  _quit <- menuQuit prj [ text := "Quit FractalStream" ]

  -- Build the help menu
  hlp   <- menuHelp      [ text := "&Help" ]
  about <- menuAbout hlp [ text := "About FractalStream" ]

  let menuBarItems = concat [ [prj], addlMenus, [hlp] ]

  set f [ menuBar := menuBarItems
        , on (menu about) :=
          infoDialog f "About FractalStream" $ unlines
          ("Contributors:" : contributors ++
            ["", "Build info:", gitBranch ++ "@" ++ take 8 gitHash])
        ]
