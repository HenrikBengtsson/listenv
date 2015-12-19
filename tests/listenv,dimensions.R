library("listenv")

ovars <- ls(envir=globalenv())
oopts <- options(warn=1)


message("* List environment and multiple dimensions ...")

x0 <- as.list(1:6)
x <- as.listenv(x0)
print(x)
stopifnot(is.null(dim(x)))
stopifnot(is.null(dimnames(x)))
y <- as.list(x)
stopifnot(identical(y, x0))
z <- as.listenv(y)
stopifnot(all.equal(z, x))

message("* dim(x) and dimnames(x) ...")
dim(x0) <- c(2,3)
dim(x) <- dim(x0)
print(x)
stopifnot(identical(dim(x), dim(x0)))
stopifnot(is.null(dimnames(x)))
y <- as.list(x)
stopifnot(identical(y, x0))
z <- as.listenv(y)
stopifnot(all.equal(z, x))

dimnames(x0) <- list(c("a", "b"), NULL)
dimnames(x) <- dimnames(x0)
print(x)
stopifnot(identical(dim(x), dim(x0)))
stopifnot(identical(dimnames(x), dimnames(x0)))
y <- as.list(x)
stopifnot(identical(y, x0))
z <- as.listenv(y)
stopifnot(all.equal(z, x))

dimnames(x0) <- NULL
dimnames(x) <- NULL
stopifnot(is.null(dimnames(x)))
y <- as.list(x)
stopifnot(identical(y, x0))
z <- as.listenv(y)
stopifnot(all.equal(z, x))

dimnames(x0) <- list(c("a", "b"), NULL)
dimnames(x) <- dimnames(x0)

dim(x0) <- NULL
dim(x) <- NULL
print(x)
stopifnot(is.null(dim(x)))
stopifnot(is.null(dimnames(x)))
y <- as.list(x)
stopifnot(identical(y, x0))
z <- as.listenv(y)
stopifnot(all.equal(z, x))


## Extract single element
message("* y <- x[[i,j]] and z <- x[i,j] ...")
dim(x) <- c(2,3)
dimnames(x) <- list(c("a", "b"), NULL)

y <- x[[3]]
stopifnot(identical(y, 3L))
z <- x[3]
stopifnot(identical(z[[1]], y))

y <- x[[1,1]]
stopifnot(identical(y, x[[1]]))
z <- x[1,1]
stopifnot(identical(z[[1]], y))

y <- x[[2,3]]
stopifnot(identical(y, x[[6]]))
z <- x[2,3]
stopifnot(identical(z[[1]], y))

y <- x[["a",3]]
stopifnot(identical(y, x[[1,3]]))
stopifnot(identical(y, x[[5]]))
z <- x["a",3]
stopifnot(identical(z[[1]], y))


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


message("* Exceptions ...")
x <- listenv()
res <- try(dim(x) <- c(2,3), silent=TRUE)
stopifnot(inherits(res, "try-error"))

length(x) <- 6
dim(x) <- c(2,3)
res <- try(dimnames(x) <- NA)
stopifnot(inherits(res, "try-error"))

res <- try(dimnames(x) <- list("a", "b", "c"))
stopifnot(inherits(res, "try-error"))

res <- try(dimnames(x) <- list("a", NULL))
stopifnot(inherits(res, "try-error"))

dimnames(x) <- list(c("a", "b"), NULL)


message("* Changing dim(x) and dimnames(x) ...")
x <- listenv()
x[1:12] <- 1:12
dim(x) <- c(2,2,3)
dimnames(x) <- list(c("a", "b"), NULL, NULL)
print(x)
stopifnot(identical(dim(x), c(2L,2L,3L)))
stopifnot(identical(dimnames(x), list(c("a", "b"), NULL, NULL)))
x[[2,1,2]] <- -x[[2,1,2]]
y <- unlist(x)
print(y)

dim(x) <- c(4,3)
print(x)
stopifnot(identical(dim(x), c(4L,3L)))
stopifnot(is.null(dimnames(x)))
x[[2,2]] <- -x[[2,2]]
y <- unlist(x)
print(y)
stopifnot(identical(y, 1:12))


message("* List environment and multiple dimensions ... DONE")


## Cleanup
options(oopts)
rm(list=setdiff(ls(envir=globalenv()), ovars), envir=globalenv())
