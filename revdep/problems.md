# aroma.affymetrix

Version: 3.1.0

## In both

*   checking CRAN incoming feasibility ... WARNING
    ```
    Maintainer: ‘Henrik Bengtsson <henrikb@braju.com>’
    
    Insufficient package version (submitted: 3.1.0, existing: 3.1.0)
    
    The Date field is over a month old.
    
    This build time stamp is over a month old.
    ```

# aroma.core

Version: 3.1.1

## In both

*   checking CRAN incoming feasibility ... WARNING
    ```
    Maintainer: ‘Henrik Bengtsson <henrikb@braju.com>’
    
    Insufficient package version (submitted: 3.1.1, existing: 3.1.1)
    
    Suggests or Enhances not in mainstream repositories:
      sfit, expectile, HaarSeg, mpcbs
    Availability using Additional_repositories specification:
      sfit        yes   https://henrikbengtsson.github.io/drat
      expectile   yes   http://r-forge.r-project.org          
      HaarSeg     yes   http://r-forge.r-project.org          
      mpcbs       yes   http://r-forge.r-project.org          
    
    The Date field is over a month old.
    
    This build time stamp is over a month old.
    ```

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      ‘sfit’ ‘expectile’ ‘HaarSeg’ ‘mpcbs’
    ```

# future

Version: 1.6.2

## In both

*   checking CRAN incoming feasibility ... WARNING
    ```
    Maintainer: ‘Henrik Bengtsson <henrikb@braju.com>’
    
    Insufficient package version (submitted: 1.6.2, existing: 1.6.2)
    
    This build time stamp is over a month old.
    ```

# future.BatchJobs

Version: 0.15.0

## In both

*   checking CRAN incoming feasibility ... WARNING
    ```
    Maintainer: ‘Henrik Bengtsson <henrikb@braju.com>’
    
    Insufficient package version (submitted: 0.15.0, existing: 0.15.0)
    
    This build time stamp is over a month old.
    ```

# future.batchtools

Version: 0.6.0

## In both

*   checking CRAN incoming feasibility ... WARNING
    ```
    Maintainer: ‘Henrik Bengtsson <henrikb@braju.com>’
    
    Insufficient package version (submitted: 0.6.0, existing: 0.6.0)
    
    This build time stamp is over a month old.
    ```

# origami

Version: 0.8.0

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > library(origami)
      > 
      > test_check("origami")
      ── 1. Error: (unknown) (@test_overall_ts.R#2)  ─────────────────────────────────
      there is no package called 'forecast'
      1: library(forecast) at testthat/test_overall_ts.R:2
      2: stop(txt, domain = NA)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 22 SKIPPED: 0 FAILED: 1
      1. Error: (unknown) (@test_overall_ts.R#2) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking CRAN incoming feasibility ... WARNING
    ```
    Maintainer: ‘Jeremy Coyle <jeremyrcoyle@gmail.com>’
    
    Insufficient package version (submitted: 0.8.0, existing: 0.8.0)
    
    This build time stamp is over a month old.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘methods’
      All declared Imports should be used.
    ```

# PSCBS

Version: 0.63.0

## In both

*   checking examples ... ERROR
    ```
    ...
      chromosome tcnId dhId     start       end tcnNbrOfLoci tcnMean tcnNbrOfSNPs
    1          1     1    1    554484 143663981         1880  1.3916          778
    2          1     2    1 143663981 185240536          671  2.0925          275
    3          1     3    1 185240536 246679946         1111  2.6545          417
      tcnNbrOfHets dhNbrOfLoci    dhMean    c1Mean    c2Mean
    1          778         778 0.4009957 0.4167872 0.9748128
    2          275         275 0.2344486 0.8009582 1.2915418
    3          417         417 0.2819897 0.9529792 1.7015208
    > 
    > 
    > # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    > # Calling segments in allelic balance (AB)
    > # NOTE: Ideally, this should be done on whole-genome data
    > # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    > # Explicitly estimate the threshold in DH for calling AB
    > # (which be done by default by the caller, if skipped here)
    > deltaAB <- estimateDeltaAB(fit, flavor="qq(DH)", verbose=verbose)
    Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
      there is no package called ‘survival’
    Calls: estimateDeltaAB ... tryCatch -> tryCatchList -> tryCatchOne -> <Anonymous>
    Execution halted
    ```

*   checking CRAN incoming feasibility ... WARNING
    ```
    Maintainer: ‘Henrik Bengtsson <henrikb@braju.com>’
    
    Insufficient package version (submitted: 0.63.0, existing: 0.63.0)
    
    The Date field is over a month old.
    
    This build time stamp is over a month old.
    ```

