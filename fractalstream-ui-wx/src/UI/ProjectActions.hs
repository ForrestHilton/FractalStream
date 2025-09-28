module UI.ProjectActions
  ( ProjectActions(..)
  ) where

data ProjectActions = ProjectActions
  { projectOpen :: FilePath -> IO ()
  , projectEdit :: FilePath -> IO ()
  , projectNew  :: IO ()
  }
