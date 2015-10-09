library("listenv")

ovars <- ls(envir=globalenv())
oopts <- options(warn=1)


message("* List environment and multiple dimensions ...")

x <- as.listenv(as.list(1:6))
print(x)
stopifnot(is.null(dim(x)))
stopifnot(is.null(dimnames(x)))


message("* dim(x) and dimnames(x) ...")
dim(x) <- c(2,3)
print(x)
stopifnot(identical(dim(x), c(2L,3L)))
stopifnot(is.null(dimnames(x)))

dimnames(x) <- list(c("a", "b"), NULL)
print(x)
stopifnot(identical(dim(x), c(2L,3L)))
stopifnot(identical(dimnames(x), list(c("a", "b"), NULL)))


## Extract single element
message("* y <- x[[i,j]] ...")
y <- x[[3]]
stopifnot(identical(y, 3L))

y <- x[[1,1]]
stopifnot(identical(y, x[[1]]))

y <- x[[2,3]]
stopifnot(identical(y, x[[6]]))

y <- x[["a",3]]
stopifnot(identical(y, x[[1,3]]))
stopifnot(identical(y, x[[5]]))


message("* x[[i,j]] <- value ...")
## Assign single element
x[[3]] <- -x[[3]]
stopifnot(identical(x[[3]], -3L))

x[[1,1]] <- -x[[1,1]]
stopifnot(identical(x[[1]], -1L))

x[[2,3]] <- -x[[2,3]]
stopifnot(identical(x[[6]], -6L))

x[["a",3]] <- -x[["a",3]]
stopifnot(identical(x[[1,3]], -5L))


message("* List environment and multiple dimensions ... DONE")


## Cleanup
options(oopts)
rm(list=setdiff(ls(envir=globalenv()), ovars), envir=globalenv())
