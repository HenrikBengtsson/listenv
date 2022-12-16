# aroma.core

<details>

* Version: 3.3.0
* GitHub: https://github.com/HenrikBengtsson/aroma.core
* Source code: https://github.com/cran/aroma.core
* Date/Publication: 2022-11-15 18:30:13 UTC
* Number of recursive dependencies: 70

Run `revdep_details(, "aroma.core")` for more info

</details>

## In both

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      'sfit', 'expectile', 'HaarSeg', 'mpcbs'
    ```

# future.apply

<details>

* Version: 1.10.0
* GitHub: https://github.com/HenrikBengtsson/future.apply
* Source code: https://github.com/cran/future.apply
* Date/Publication: 2022-11-05 08:50:02 UTC
* Number of recursive dependencies: 15

Run `revdep_details(, "future.apply")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘fold.R’
      Running ‘future_apply.R’
      Running ‘future_by.R’
      Running ‘future_eapply.R’
      Running ‘future_lapply,RNG.R’
      Running ‘future_lapply,globals.R’
      Running ‘future_lapply.R’
     ERROR
    Running the tests in ‘tests/future_lapply.R’ failed.
    Last 50 lines of output:
    ...
      A 'listenv' vector with 1 element ('A').
      
      $b
      A 'listenv' vector with 2 elements ('A', 'B').
      
      > y_c <- lapply(x_c, FUN = listenv::map)
      Error: 'FUN' is defunct.
      Use 'listenv::mapping()' instead.
      See help("Defunct") and help("listenv-defunct").
      Execution halted
    ```

# future.batchtools

<details>

* Version: 0.11.0
* GitHub: https://github.com/HenrikBengtsson/future.batchtools
* Source code: https://github.com/cran/future.batchtools
* Date/Publication: 2022-12-14 21:40:03 UTC
* Number of recursive dependencies: 38

Run `revdep_details(, "future.batchtools")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘BatchtoolsFuture,gc.R’
      Running ‘BatchtoolsFuture.R’
      Running ‘BatchtoolsFutureError.R’
      Running ‘batchtools_custom.R’
      Running ‘batchtools_hpc.R’
      Running ‘batchtools_interactive.R’
      Running ‘batchtools_local.R’
      Running ‘batchtools_multicore.R’
      Running ‘batchtools_ssh.R’
      Running ‘batchtools_template.R’
    ...
      
      $b
      A 'listenv' vector with 2 elements ('A', 'B').
      
      > 
      > y0 <- lapply(x, FUN = listenv::map)
      Error: 'FUN' is defunct.
      Use 'listenv::mapping()' instead.
      See help("Defunct") and help("listenv-defunct").
      Execution halted
    ```

# future.callr

<details>

* Version: 0.8.1
* GitHub: https://github.com/HenrikBengtsson/future.callr
* Source code: https://github.com/cran/future.callr
* Date/Publication: 2022-12-14 17:10:09 UTC
* Number of recursive dependencies: 20

Run `revdep_details(, "future.callr")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘CallrFuture.R’
      Running ‘callr,launch-failure.R’
      Running ‘callr.R’
      Running ‘demo.R’
      Running ‘dotdotdot.R’
      Running ‘future,labels.R’
      Running ‘future,lazy.R’
      Running ‘globals,formulas.R’
      Running ‘globals,manual.R’
      Running ‘globals,subassignment.R’
    ...
      
      $b
      A 'listenv' vector with 2 elements ('A', 'B').
      
      > 
      > y0 <- lapply(x, FUN = listenv::map)
      Error: 'FUN' is defunct.
      Use 'listenv::mapping()' instead.
      See help("Defunct") and help("listenv-defunct").
      Execution halted
    ```

# greed

<details>

* Version: 0.6.1
* GitHub: https://github.com/comeetie/greed
* Source code: https://github.com/cran/greed
* Date/Publication: 2022-10-03 22:00:05 UTC
* Number of recursive dependencies: 95

Run `revdep_details(, "greed")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 37.0Mb
      sub-directories of 1Mb or more:
        libs  34.8Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 989 marked UTF-8 strings
    ```

# signeR

<details>

* Version: 2.0.1
* GitHub: https://github.com/rvalieris/signeR
* Source code: https://github.com/cran/signeR
* Date/Publication: 2022-12-14
* Number of recursive dependencies: 242

Run `revdep_details(, "signeR")` for more info

</details>

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.7Mb
      sub-directories of 1Mb or more:
        R     1.1Mb
        doc   4.7Mb
    ```

*   checking R code for possible problems ... NOTE
    ```
    covariate: no visible binding for global variable ‘.’
    denovo: no visible binding for global variable
      ‘BSgenome.Hsapiens.UCSC.hg19’
    denovo: no visible binding for global variable
      ‘BSgenome.Hsapiens.UCSC.hg38’
    explorepage: no visible binding for global variable ‘.’
    fitting: no visible binding for global variable
      ‘BSgenome.Hsapiens.UCSC.hg19’
    fitting: no visible binding for global variable
      ‘BSgenome.Hsapiens.UCSC.hg38’
    ...
    ExposureCorrelation,SignExp-numeric: no visible binding for global
      variable ‘exposure’
    ExposureCorrelation,matrix-numeric: no visible binding for global
      variable ‘Feature’
    ExposureCorrelation,matrix-numeric: no visible binding for global
      variable ‘exposure’
    Undefined global functions or variables:
      . BSgenome.Hsapiens.UCSC.hg19 BSgenome.Hsapiens.UCSC.hg38 Col Feature
      Frequency Row Samples Signatures alt<- exposure fc project sig
      sig_test
    ```

*   checking Rd files ... NOTE
    ```
    prepare_Rd: cosmic_data.Rd:91-93: Dropping empty section \details
    prepare_Rd: cosmic_data.Rd:98-100: Dropping empty section \references
    prepare_Rd: cosmic_data.Rd:101-102: Dropping empty section \examples
    prepare_Rd: tcga_similarities.Rd:96-98: Dropping empty section \details
    prepare_Rd: tcga_similarities.Rd:99-101: Dropping empty section \source
    prepare_Rd: tcga_similarities.Rd:102-104: Dropping empty section \references
    prepare_Rd: tcga_similarities.Rd:105-106: Dropping empty section \examples
    prepare_Rd: tcga_tumors.Rd:18-20: Dropping empty section \details
    prepare_Rd: tcga_tumors.Rd:21-23: Dropping empty section \source
    prepare_Rd: tcga_tumors.Rd:24-26: Dropping empty section \references
    prepare_Rd: tcga_tumors.Rd:27-28: Dropping empty section \examples
    ```

