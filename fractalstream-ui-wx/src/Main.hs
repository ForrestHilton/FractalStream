{- |
Module       : Main
Description  : Main entry point into FractalStream
-}
module Main where

import Actor.Ensemble

import UI.ProjectActions
import UI.Menu
import UI.ProjectViewer (viewProject)
import UI.ProjectEditor (editProject)

import qualified Data.Yaml as YAML

import UI.Welcome

import Backend
import Graphics.UI.WX (start)

main :: IO ()
main = withBackend $ \complexViewerCompiler -> start $ do

  let projectNew = putStrLn "TODO"

      projectOpen = \yamlFile -> do
        -- TODO add error handling instead of throwing here
        ensemble <- YAML.decodeFileThrow yamlFile
        runEnsemble complexViewerCompiler
                    (viewProject (makeMenuBar ProjectActions{..}))
                    ensemble

      projectEdit = editProject

  welcome ProjectActions{..}
