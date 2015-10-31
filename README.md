# listenv: Environments Behaving (Almost) as Lists

## Introduction
_List environments_ are environments that have list-like subsetting
properties such as indexed subsetting.
For example,

```r
> x <- listenv(a=2, b=3, c="hello")
> x$a
[1] 2
> x$d <- x$a + x[["b"]]
> x[[3]] <- toupper(x[[3]])
> length(x)
[1] 4
> seq_along(x)
[1] 1 2 3 4
> names(x)
[1] "a" "b" "c" "d"
> x$c
[1] "HELLO"
> x[[4]]
[1] 5
```

### Coercing to lists and vectors

Coercing a list environment to a list:
```r
> x <- listenv(a=2, b=3, c="hello")
`listenv` with 3 elements: 'a', 'b', 'c'
> y <- as.list(x)
> str(y)
List of 4
 $ a: num 2
 $ b: num 3
 $ c: chr "hello"
```

Coercing a list to a list environment:
```r
> z <- as.listenv(y)
`listenv` with 3 elements: 'a', 'b', 'c'
> all.equal(z, x)
[1] TRUE
```

Coercing a list environment to a vector ("unlisting"):
```r
> unlist(x)
      a       b       c
    "2"     "3" "hello"
> unlist(x[-3])
a b
2 3
> unlist(x[1:2], use.names=FALSE)
[1] 2 3
```


### List environment with initial set of elements

```r
> x <- listenv(a=1, b=c("hello", "world"), c=list(d=42L, e=pi))
> x
`listenv` with 3 elements: 'a', 'b', 'c'
> names(x)
> str(as.list(x))
List of 3
 $ a: num 1
 $ b: chr [1:2] "hello" "world"
 $ c:List of 2
  ..$ d: int 42
  ..$ e: num 3.14
```


### Empty list environment with given length
```r
> x <- listenv()
> length(x) <- 3
> length(x)
[1] 3
> names(x)
NULL
> str(as.list(x))
List of 3
 $ : NULL
 $ : NULL
 $ : NULL
```

### Assigning names to elements
```r
> x <- listenv()
> length(x) <- 3
> names(x) <- letters[1:3]
> names(x)
[1] "a" "b" "c"
> names(x)[2] <- "B"
> str(as.list(x))
List of 3
 $ a: NULL
 $ B: NULL
 $ c: NULL
```

## Methods
```r
> x <- listenv()
> length(x) <- 3
> seq_along(x)
[1] 1 2 3
```


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
> x[['b']]
NULL
> x$b <- TRUE
> x[[1]]
1
> str(as.list(x))
List of 3
 $ a: num 1
 $ b: logi TRUE
 $ c: chr "Hello world!"
> x[c('a', 'c')] <- list(2, "Hello again!")
> y <- x[3:2]
> str(as.list(y))
List of 2
 $ c: chr "Hello again!"
 $ b: logi TRUE
```


[listenv]: http://cran.r-project.org/package=listenv


## Installation
R package listenv is available on [CRAN](http://cran.r-project.org/package=listenv) and can be installed in R as:
```r
install.packages('listenv')
```

### Pre-release version

To install the pre-release version that is available in branch `develop`, use:
```r
source('http://callr.org/install#HenrikBengtsson/listenv@develop')
```
This will install the package from source.  



## Software status

| Resource:     | CRAN        | Travis CI     | Appveyor         |
| ------------- | ------------------- | ------------- | ---------------- |
| _Platforms:_  | _Multiple_          | _Linux_       | _Windows_        |
| R CMD check   | <a href="http://cran.r-project.org/web/checks/check_results_listenv.html"><img border="0" src="http://www.r-pkg.org/badges/version/listenv" alt="CRAN version"></a> | <a href="https://travis-ci.org/HenrikBengtsson/listenv"><img src="https://travis-ci.org/HenrikBengtsson/listenv.svg" alt="Build status"></a> | <a href="https://ci.appveyor.com/project/HenrikBengtsson/listenv"><img src="https://ci.appveyor.com/api/projects/status/github/HenrikBengtsson/listenv?svg=true" alt="Build status"></a> |
| Test coverage |                     | <a href="https://coveralls.io/r/HenrikBengtsson/listenv"><img src="https://coveralls.io/repos/HenrikBengtsson/listenv/badge.svg?branch=develop" alt="Coverage Status"/></a>   |                  |
