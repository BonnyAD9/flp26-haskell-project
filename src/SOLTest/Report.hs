-- | Building the final test report and computing statistics.
--
-- This module assembles a 'TestReport' from the results of test execution,
-- computes aggregate statistics, and builds the per-category success-rate
-- histogram.
module SOLTest.Report
  ( buildReport,
    groupByCategory,
    computeStats,
    computeHistogram,
    rateToBin,
  )
where

import Data.Map.Strict (Map, empty, foldl', fromList, insertWith, lookup, singleton, toList, union)
import SOLTest.Types

-- ---------------------------------------------------------------------------
-- Top-level report assembly
-- ---------------------------------------------------------------------------

-- | Assemble the complete 'TestReport'.
--
-- Parameters:
--
-- * @discovered@ – all 'TestCaseDefinition' values that were successfully parsed.
-- * @unexecuted@ – tests that were not executed for any reason (filtered, malformed, etc.).
-- * @executionResults@ – 'Nothing' in dry-run mode; otherwise the map of test
--   results keyed by test name.
-- * @selected@ – the tests that were selected for execution (used for stats).
-- * @foundCount@ – total number of @.test@ files discovered on disk.
buildReport ::
  [TestCaseDefinition] ->
  Map String UnexecutedReason ->
  Maybe (Map String TestCaseReport) ->
  [TestCaseDefinition] ->
  Int ->
  TestReport
buildReport discovered unexecuted mResults selected foundCount =
  let mCategoryResults = fmap (groupByCategory selected) mResults
      stats = computeStats foundCount (length discovered) (length selected) mCategoryResults
   in TestReport
        { trDiscoveredTestCases = discovered,
          trUnexecuted = unexecuted,
          trResults = mCategoryResults,
          trStats = stats
        }

-- ---------------------------------------------------------------------------
-- Grouping and category reports
-- ---------------------------------------------------------------------------

-- | Group a flat map of test results into a map of 'CategoryReport' values,
-- one per category.
--
-- The @definitions@ list is used to look up each test's category and points.
--
-- FLP: Implement this function. The following functions may (or may not) come in handy:
--      @Map.fromList@, @Map.foldlWithKey'@, @Map.empty@, @Map.lookup@, @Map.insertWith@,
--      @Map.map@, @Map.fromList@
groupByCategory ::
  [TestCaseDefinition] ->
  Map String TestCaseReport ->
  Map String CategoryReport
groupByCategory definitions results =
  groupByCategory2
    (fromList $ Prelude.map (\x -> (tcdName x, tcdCategory x)) definitions)
    (toList results)
    empty

-- | Inner implementation of groupByCategory. Incrementaly builds the resulting map.
groupByCategory2 :: Map String TestCategory -> [(String, TestCaseReport)] -> Map String CategoryReport -> Map String CategoryReport
groupByCategory2 _ [] res = res
groupByCategory2 tcm rr@((n, _) : _) res =
  groupByCategory3 tcm rr res $
    Data.Map.Strict.lookup n tcm

-- | Inner implementation of groupByCategory2. Takes the TestCategory as maybe.
groupByCategory3 :: Map String TestCategory -> [(String, TestCaseReport)] -> Map String CategoryReport -> Maybe TestCategory -> Map String CategoryReport
groupByCategory3 tcm (_ : rr) res Nothing = groupByCategory2 tcm rr res
groupByCategory3 tcm ((n, r) : rr) res (Just c) =
  groupByCategory2 tcm rr $
    insertWith combineReports c (makeCategoryReport n r) res
groupByCategory3 _ [] res _ = res

-- | Combines to category reports.
combineReports :: CategoryReport -> CategoryReport -> CategoryReport
combineReports (CategoryReport atp ap ar) (CategoryReport btp bp br) =
  CategoryReport (atp + btp) (ap + bp) $ union ar br

-- | Creates category report from test case report.
makeCategoryReport :: String -> TestCaseReport -> CategoryReport
makeCategoryReport n tcr = case tcrResult tcr of
  Passed -> CategoryReport 1 1 r
  _ -> CategoryReport 1 0 r
  where
    r = singleton n tcr

-- ---------------------------------------------------------------------------
-- Statistics
-- ---------------------------------------------------------------------------

-- | Compute the 'TestStats' from available information.
--
-- FLP: Implement this function. You'll use @computeHistogram@ here.
computeStats ::
  -- | Total @.test@ files found on disk.
  Int ->
  -- | Number of successfully parsed tests.
  Int ->
  -- | Number of tests selected after filtering.
  Int ->
  -- | Category reports (Nothing in dry-run mode).
  Maybe (Map String CategoryReport) ->
  TestStats
computeStats foundCount loadedCount selectedCount Nothing =
  TestStats foundCount loadedCount selectedCount 0 empty
computeStats fc lc sc (Just cr) = TestStats fc lc sc (countAllPassed cr) $ computeHistogram cr

-- | Count the number of passed tests inside category reports.
countAllPassed :: Map String CategoryReport -> Int
countAllPassed = foldl' (\c m -> c + countPassed m) 0

-- | Count the number of passed test insie category report.
countPassed :: CategoryReport -> Int
countPassed = foldl' (\c m -> c + if tcrResult m == Passed then 1 else 0) 0 . crTestResults

-- ---------------------------------------------------------------------------
-- Histogram
-- ---------------------------------------------------------------------------

-- | Compute the success-rate histogram from the category reports.
--
-- For each category, the relative pass rate is:
--
-- @rate = passed_test_count \/ total_test_count@
--
-- The rate is mapped to a bin key (@\"0.0\"@ through @\"0.9\"@) and the count
-- of categories in each bin is accumulated. All ten bins are always present in
-- the result, even if their count is 0.
--
-- FLP: Implement this function.
computeHistogram :: Map String CategoryReport -> Map String Int
computeHistogram = foldl' (\h cr -> insertWith (+) (rateToBin $ getRate cr) 1 h) empty

getRate :: CategoryReport -> Double
getRate cr = (fromIntegral . countPassed) cr / (fromIntegral . length . crTestResults) cr

-- | Map a pass rate in @[0, 1]@ to a histogram bin key.
--
-- Bins are defined as @[0.0, 0.1)@, @[0.1, 0.2)@, ..., @[0.9, 1.0]@.
-- A rate of exactly @1.0@ maps to the @\"0.9\"@ bin.
rateToBin :: Double -> String
rateToBin rate =
  let binIndex = min 9 (floor (rate * 10) :: Int)
      -- Format as "0.N" for bin index N
      whole = binIndex `div` 10
      frac = binIndex `mod` 10
   in show whole ++ "." ++ show frac
