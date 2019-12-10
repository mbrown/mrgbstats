# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## [Unreleased] - 2019-12-09

### Changes
- In mrgbstats.core, permtest fn renamed to permtest-singlevar and now takes
  single variable functions func-a and func-b (eg: mean, variance, etc.) that
  compute scores to compare from groups a and b, respectively. Can pass the
  same function for both func-a and func-b if desired.
- In mrgbstats.core, added permtest-mean fn for comparing means of two groups.
- In mrgbstats.core, added permtest-multiprops fn for comparing multiple
  proportions of values from two groups.

## Version 0.1.1
- 0.1.1-SNAPSHOT version started 2019-12-09

## Version 0.1.0 - 2019-07-15
- Initial version
- Permutation testing to compare means of two groups (mrgbstats.core/permtest
  fn)
- FDR multiple comparison correction (mrgbstats.core/fdr fn)

## Version 0.1.0-SNAPSHOT - 2019-07-11
- SNAPSHOT version created 2019-07-11

