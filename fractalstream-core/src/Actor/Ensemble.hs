{-# language OverloadedStrings #-}
module Actor.Ensemble
  ( Ensemble(..)
  , runEnsemble
  ) where

import FractalStream.Prelude

import Data.DynamicValue
import Actor.UI
import Actor.Configuration
import Actor.Layout
import Actor.Viewer.Complex
import Language.Environment
import Language.Value.Parser (Splices)

import Data.Aeson
import qualified Data.Map as Map

data Ensemble = Ensemble
  { ensembleSetup :: Maybe Configuration
  , ensembleConfiguration :: Maybe Configuration
  , ensembleViewers :: [ComplexViewer]
  }
  deriving Show

instance FromJSON Ensemble where
  parseJSON = withObject "ensemble" $ \o -> do
    ensembleSetup <- o .:? "setup"
    ensembleConfiguration <- o .:? "configuration"
    singleViewer <- o .:? "viewer"
    ensembleViewers <- case singleViewer of
      Just viewer -> pure [viewer]
      Nothing -> o .:? "viewers" .!= []
    pure Ensemble{..}

runEnsemble :: ComplexViewerCompiler
            -> UI
            -> Ensemble
            -> IO ()
runEnsemble jit UI{..} Ensemble{..} = do

  -- Get a handle for the ensemble
  project <- newEnsemble

  -- Make the setup window and let it run
  let withSplicesFromSetup :: (Splices -> IO ())
                           -> IO ()
      withSplicesFromSetup k = case ensembleSetup of
        Nothing -> k Map.empty
        Just setup -> do
          setupUI <- runExceptTIO (allocateUIExpressions (coContents setup))
          runSetup project (coTitle setup) (toSomeDynamic setupUI) (withSplices setupUI k)

      withContextFromConfiguration :: (forall env. Context DynamicValue env -> IO ())
                                   -> IO ()
      withContextFromConfiguration k = case ensembleConfiguration of
        Nothing -> k EmptyContext
        Just config -> do
          configUI <- runExceptTIO (allocateUIConstants (coContents config))
          makeLayout project (coTitle config) (toSomeDynamic configUI)
          withDynamicBindings configUI k

  withSplicesFromSetup $ \splices -> do
    withContextFromConfiguration $ \config -> do
      ProofNameIsAbsent <- assertAbsentInEnv' (Proxy @"[internal argument] #blockWidth")
                                              (contextToEnv config) "internal error"
      ProofNameIsAbsent <- assertAbsentInEnv' (Proxy @"[internal argument] #blockHeight")
                                              (contextToEnv config) "internal error"
      ProofNameIsAbsent <- assertAbsentInEnv' (Proxy @"[internal argument] #subsamples")
                                              (contextToEnv config) "internal error"
      forM_ ensembleViewers $ \viewer ->
        withComplexViewer' jit config splices viewer $ \vu cv' -> do
          makeViewer project vu cv'


runExceptTIO :: ExceptT String IO a -> IO a
runExceptTIO = fmap (either error id) . runExceptT
