-- | Discovering @.test@ files and their companion @.in@\/@.out@ files.
module SOLTest.Discovery (discoverTests) where

import SOLTest.Types
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    listDirectory,
  )
import System.FilePath (replaceExtension, takeBaseName, takeExtension, (</>))

-- | Discover all @.test@ files in a directory.
--
-- When @recursive@ is 'True', subdirectories are searched recursively.
-- Returns a list of 'TestCaseFile' records, one per @.test@ file found.
-- The list is ordered by the file system traversal order (not sorted).
--
-- FLP: Implement this function. The following functions may come in handy:
--      @doesDirectoryExist@, @takeExtension@, @forM@ or @mapM@,
--      @findCompanionFiles@ (below).
discoverTests :: Bool -> FilePath -> IO [TestCaseFile]
discoverTests recursive dir = do
  entries <- listDirectory dir
  let fullPaths = map (dir </>) entries
  tests <- mapM (checkPath recursive) fullPaths
  return $ concat tests

-- | Gets all tests coresponding to the given path.
--
-- If the path is test file, it constructs the test. If it is directory and
-- recursive is true, all tests in the directory are returned. Otherwise empty
-- array is returned.
checkPath :: Bool -> FilePath -> IO [TestCaseFile]
checkPath recursive p
  | takeExtension p == ".test" = findCompanionFiles p >>= \x -> return [x]
  | recursive = do
      isDir <- doesDirectoryExist p
      if isDir then discoverTests recursive p else return []
  | otherwise = return []

-- | Build a 'TestCaseFile' for a given @.test@ file path, checking for
-- companion @.in@ and @.out@ files in the same directory.
findCompanionFiles :: FilePath -> IO TestCaseFile
findCompanionFiles testPath = do
  let baseName = takeBaseName testPath
      inFile = replaceExtension testPath ".in"
      outFile = replaceExtension testPath ".out"
  hasIn <- doesFileExist inFile
  hasOut <- doesFileExist outFile
  return
    TestCaseFile
      { tcfName = baseName,
        tcfTestSourcePath = testPath,
        tcfStdinFile = if hasIn then Just inFile else Nothing,
        tcfExpectedStdout = if hasOut then Just outFile else Nothing
      }
