{-# language TemplateHaskell #-}

module UI.Menu
  ( makeMenuBar
  ) where

import UI.ProjectActions

import qualified System.Info as Info
import Control.Monad
import Development.IncludeFile
import qualified Data.ByteString.UTF8 as Utf8

import Graphics.UI.WX hiding (when)

$(includeFileInSource "../CONTRIBUTORS" "contributorsFileContents")

contributors :: [String]
contributors = lines (Utf8.toString contributorsFileContents)

makeMenuBar :: ProjectActions -> Frame a -> [Menu ()] -> IO ()
makeMenuBar ProjectActions{..} f addlMenus = do

  let getProject verb k = maybe (pure ()) k =<<
        fileOpenDialog objectNull True True
           (verb ++ " a FractalStream 2 project")
           [ ("FractalStream 2 project files", ["*.yaml"])
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

  when (Info.os /= "darwin" || True) $ do
    void $ menuQuit prj [ text := "&Quit"
                        , help := "Quit FractalStream" ]

  -- Build the help menu
  hlp   <- menuHelp      [ text := "&Help" ]
  about <- menuAbout hlp [ text := "About FractalStream" ]

  let menuBarItems = concat [ [prj], addlMenus, [hlp] ]

  set f [ menuBar := menuBarItems
        , on (menu about) :=
            infoDialog f "About FractalStream" $ unlines
              ("Contributors:" : contributors)
        ]
