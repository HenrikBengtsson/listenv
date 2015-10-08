library("listenv")

ovars <- ls(envir=globalenv())
oopts <- options(warn=1)


x <- as.listenv(as.list(1:6))
print(x)
stopifnot(is.null(dim(x)))
stopifnot(is.null(dimnames(x)))

dim(x) <- c(2,3)
print(x)
stopifnot(identical(dim(x), c(2L,3L)))
stopifnot(is.null(dimnames(x)))

dimnames(x) <- list(c("a", "b"), NULL)
print(x)
stopifnot(identical(dim(x), c(2L,3L)))
stopifnot(identical(dimnames(x), list(c("a", "b"), NULL)))

## Extract single element
y <- x[[3]]
stopifnot(identical(y, 3L))

y <- x[[1,1]]
stopifnot(identical(y, x[[1]]))

y <- x[[2,3]]
stopifnot(identical(y, x[[6]]))

y <- x[["a",3]]
stopifnot(identical(y, x[[1,3]]))
stopifnot(identical(y, x[[5]]))


## Cleanup
options(oopts)
rm(list=setdiff(ls(envir=globalenv()), ovars), envir=globalenv())
