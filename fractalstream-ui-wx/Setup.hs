import Distribution.MacOSX
import Distribution.Simple

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks {
         postBuild = appBundleBuildHook guiApps -- no-op if not MacOS X
       }

guiApps :: [MacApp]
guiApps = [MacApp "FractalStream"

                  -- Icon file
                  (Just "FS.icns")

                  -- Build a default Info.plist
                  Nothing

                  -- Other resource files
                  []

                  -- Other binary files
                  []

                  -- Starting in Big Sur, MacOS caches certain
                  -- system libraries. They act like they are present
                  -- when using dlopen but do not actually exist on
                  -- disk at the stated locations! This throws off
                  -- `ChaseWithDefaults` when it uses otool -L to find
                  -- dylib dependencies. We'll work around it by exlcuding
                  -- /usr/lib, where these libraries claim to be installed.
                  --
                  -- See: https://developer.apple.com/forums/thread/655588
                  --
                  (ChaseWith $ defaultExclusions ++ ["/usr/lib"])
          ]
