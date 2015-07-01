# listenv: Environments Behaving (Almost) as Lists

Copyright Henrik Bengtsson, 2015

## List environments
_List environments_ are environments that behaves as lists by
overriding the subsetting functions for environments such that they
also emulates some of the index subsetting that lists have.  For example,
```r
x <- listenv()
for (i in 1:3) {
  x[[i]] <- i^2
}
names(x) <- c("a", "b", "c")
```
The values of a list environment can be retrieved individually via
`x$b` and `x[["b"]]` just as with regular environments, but also via
`x[[2]]` as with regular lists.
To retrieve all values of an environment as a list, use `as.list(x)`.

### Examples
Here is a longer set of examples illustrating what the list environments provides:
```r
> x <- listenv()
> x[[1]] <- { 1 }
> x[[3]] <- { "Hello world!" }
> length(x)
3
> seq_along(x)
[1] 1 2 3
> names(x) <- c("a", "b", "c")
> x$b <- TRUE
> x[[1]]
1
> as.list(x)
$a
[1] 1

$b
[1] TRUE

$c
[1] "Hello world!"
```

It is possible to also specify the length upfront, e.g.
```r
> x <- listenv(length=4)
> seq_along(x)
[1] 1 2 3 4
```

## Installation
R package listenv is available on [CRAN](http://cran.r-project.org/package=listenv) and can be installed in R as:
```r
install.packages('listenv')
```


## Software status

| Resource:     | CRAN        | Travis CI     | Appveyor         |
| ------------- | ------------------- | ------------- | ---------------- |
| _Platforms:_  | _Multiple_          | _Linux_       | _Windows_        |
| R CMD check   | <a href="http://cran.r-project.org/web/checks/check_results_listenv.html"><img border="0" src="http://www.r-pkg.org/badges/version/listenv" alt="CRAN version"></a> | <a href="https://travis-ci.org/HenrikBengtsson/listenv"><img src="https://travis-ci.org/HenrikBengtsson/listenv.svg" alt="Build status"></a> | <a href="https://ci.appveyor.com/project/HenrikBengtsson/listenv"><img src="https://ci.appveyor.com/api/projects/status/github/HenrikBengtsson/listenv?svg=true" alt="Build status"></a> |
| Test coverage |                     | <a href="https://coveralls.io/r/HenrikBengtsson/listenv"><img src="https://coveralls.io/repos/HenrikBengtsson/listenv/badge.svg?branch=develop" alt="Coverage Status"/></a>   |                  |
