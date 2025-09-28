module UI.Welcome
  ( welcome
  ) where

import Graphics.UI.WX

import UI.ProjectActions

welcome :: ProjectActions -> IO ()
welcome ProjectActions{..} = do

  f <- frame [ text := "Welcome to FractalStream!"
             , on resize := propagateEvent ]

  let getProject verb action = do
        mprj <- fileOpenDialog objectNull True True
                (verb ++ " a FractalStream project")
                [ ("FractalStream project files", ["*.yaml"])
                , ("All files", ["*.*"])
                ] "" ""
        case mprj of
          Nothing -> pure ()
          Just prj -> do
            set f [ visible := False ]
            action prj

  -- TODO: verify that the code for each viewer, tool, etc works properly
  --       with the splices declared by the setup config. e.g. all code
  --       typechecks with the splice's types, each splice's environment
  --       is contained in the actual code environment at each use, etc.
  --
  --       If the ensemble passes this verification, then the end-user
  --       should not be able to cause a compilation failure via the
  --       UI.
  new  <- button f [ text := "New project"
                   , on command := projectNew ]
  open <- button f [ text := "Open project"
                   , on command := getProject "Open" projectOpen
                   ]
  edit <- button f [ text := "Edit project"
                   , on command := getProject "Edit" projectEdit
                   ]
  set f [ layout := fill . margin 5 . column 5
          $ [ widget new, widget open, widget edit ]]
