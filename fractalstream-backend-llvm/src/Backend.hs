module Backend
  ( withBackend
  ) where

import Actor.Viewer.Complex
import Backend.LLVM

withBackend :: (ComplexViewerCompiler -> IO a) -> IO a
withBackend action = withJIT $ \jit ->
  action (ComplexViewerCompiler (withViewerCode' jit))
