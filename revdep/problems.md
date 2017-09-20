# aroma.affymetrix

Version: 3.1.0

## In both

*   checking CRAN incoming feasibility ... NOTE
    ```
    Maintainer: 'Henrik Bengtsson <henrikb@braju.com>'
    
    The Date field is over a month old.
    
    This build time stamp is over a month old.
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 1028.1Mb
      sub-directories of 1Mb or more:
        R            1024.9Mb
        help            1.1Mb
        testScripts     1.2Mb
    ```

# aroma.core

Version: 3.1.1

## In both

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      'GLAD' 'sfit' 'expectile' 'HaarSeg' 'mpcbs'
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 1025.6Mb
      sub-directories of 1Mb or more:
        R  1024.5Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: 'GLAD'
    ```

# future

Version: 1.6.1

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in 'tests/futures.R' failed.
    Last 13 lines of output:
      - args: function (expr, envir = parent.frame(), substitute = TRUE, lazy = FALSE, seed = NULL, globals = TRUE, persistent = FALSE, workers = availableWorkers(), user = NULL, revtunnel = TRUE, homogeneous = TRUE, gc = FALSE, earlySignal = FALSE, label = NULL, ...)
      - tweaked: FALSE
      - call: plan(strategy)
      Workers: [n = 2] 'localhost', 'localhost'
      Base port: 11215
      Creating node 1 of 2 ...
      - setting up node
      Starting worker #1 on 'localhost': '/home/shared/cbc/software_cbc/R/R-3.4.1-20170630/lib64/R/bin/Rscript' --default-packages=datasets,utils,grDevices,graphics,stats,methods -e 'parallel:::.slaveRSOCK()' MASTER=localhost PORT=11215 OUT=/dev/null TIMEOUT=2592000 XDR=TRUE
      Waiting for worker #1 on 'localhost' to connect back
      Warning in socketConnection("localhost", port = port, server = TRUE, blocking = TRUE,  :
        port 11215 cannot be opened
      Error in socketConnection("localhost", port = port, server = TRUE, blocking = TRUE,  : 
        cannot open the connection
      Calls: plan ... makeClusterPSOCK -> makeNode -> <Anonymous> -> socketConnection
      Execution halted
    ```

# origami

Version: 0.8.0

## In both

*   checking examples ... WARNING
    ```
    checking a package with encoding  'UTF-8'  in an ASCII locale
    ```

*   checking CRAN incoming feasibility ... NOTE
    ```
    Maintainer: 'Jeremy Coyle <jeremyrcoyle@gmail.com>'
    
    This build time stamp is over a month old.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: 'methods'
      All declared Imports should be used.
    ```

# PSCBS

Version: 0.63.0

## In both

*   checking CRAN incoming feasibility ... NOTE
    ```
    Maintainer: 'Henrik Bengtsson <henrikb@braju.com>'
    
    The Date field is over a month old.
    
    This build time stamp is over a month old.
    ```

