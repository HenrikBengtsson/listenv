# CRAN submission listenv 0.9.0

on 2022-12-15

I've verified this submission has no negative impact on any of the 12 reverse package dependencies available on CRAN (n = 11) and Bioconductor (n = 1).

Thank you


## Notes not sent to CRAN

### R CMD check validation

The package has been verified using `R CMD check --as-cran` on:

| R version | GitHub | R-hub  | mac/win-builder |
| --------- | ------ | ------ | --------------- |
| 3.4.x     | L      |        |                 |
| 3.6.x     | L      |        |                 |
| 4.0.x     | L      |        |                 |
| 4.1.x     | L M W  |   M    |                 |
| 4.2.x     | L M W  | L   W  | M1 W            |
| devel     | L M W  | L      | M1 W            |

_Legend: OS: L = Linux, M = macOS, M1 = macOS M1, W = Windows_


R-hub checks:

```r
res <- rhub::check(platforms = c(
  "debian-clang-devel", 
  "fedora-gcc-devel",
  "debian-gcc-patched", 
  "macos-highsierra-release-cran",
  "windows-x86_64-release"
))
print(res)
```

gives

```
── listenv 0.9.0: OK

  Build ID:   listenv_0.9.0.tar.gz-356a259e31d9443bad573242d36c654e
  Platform:   Debian Linux, R-devel, clang, ISO-8859-15 locale
  Submitted:  14m 23.9s ago
  Build time: 14m 19.9s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

── listenv 0.9.0: OK

  Build ID:   listenv_0.9.0.tar.gz-aebda834c36644c788e89d6a7912bc36
  Platform:   Fedora Linux, R-devel, GCC
  Submitted:  14m 24s ago
  Build time: 9m 28.1s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

── listenv 0.9.0: OK

  Build ID:   listenv_0.9.0.tar.gz-b767e683e28d41a18da2e84adb2240ee
  Platform:   Debian Linux, R-patched, GCC
  Submitted:  14m 24s ago
  Build time: 13m 47.5s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

── listenv 0.9.0: OK

  Build ID:   listenv_0.9.0.tar.gz-5193cf31830b48c28f8fcc14739d3694
  Platform:   macOS 10.13.6 High Sierra, R-release, CRAN's setup
  Submitted:  14m 24s ago
  Build time: 1m 40s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

── listenv 0.9.0: OK

  Build ID:   listenv_0.9.0.tar.gz-ee73618b5f5a4b2c89033e6443f3cf17
  Platform:   Windows Server 2022, R-release, 32/64 bit
  Submitted:  14m 24s ago
  Build time: 1m 41.6s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔
```
