library("listenv")

ovars <- ls(envir = globalenv())
oopts <- options(warn = 1)


message("* List environment and multiple dimensions ...")

x <- listenv()
dim(x) <- c(0, 0)
print(x)
stopifnot(length(x) == 0)
stopifnot(is.null(dimnames(x)))
stopifnot(is.null(names(x)))

x <- listenv(a = 1)
stopifnot(identical(names(x), "a"))
dim(x) <- c(1, 1)
print(x)
stopifnot(length(x) == 1)
stopifnot(is.null(dimnames(x)))
stopifnot(is.null(names(x)))

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
dims <- list(2:3, 2:4)
for (kk in seq_along(dims)) {
  dim <- dims[[kk]]
  dimnames <- lapply(dim, FUN = function(n) letters[seq_len(n)])
  names <- letters[seq_len(prod(dim))]
  str(list(dim = dim, dimnames = dimnames, names = names))

  n <- prod(dim)
  values <- seq_len(n)

  x0 <- as.list(values)
  x <- as.listenv(values)
  print(x)
  stopifnot(identical(dim(x), dim(x0)))
  y <- as.list(x)
  stopifnot(identical(y, x0))
  z <- as.listenv(y)
  stopifnot(all.equal(z, x))

  dim(x0) <- dim
  dim(x) <- dim
  print(x)
  stopifnot(identical(dim(x), dim(x0)))
  stopifnot(is.null(dimnames(x)))
  stopifnot(is.null(names(x)))
  names(x0) <- names
  names(x) <- names
  y <- as.list(x)
  stopifnot(identical(y, x0))
  z <- as.listenv(y)
  stopifnot(all.equal(z, x))

  ## Infer one of the dimensions if given as NA
  dim0 <- dim(x)
  for (dd in seq_along(dim0)) {
    dim_dd <- dim0
    dim_dd[dd] <- NA_integer_
    dim_na(x) <- dim_dd
    print(x)
    stopifnot(identical(dim(x), dim0))
  }
  names(x) <- names
  
  excls <- c(list(NULL), as.list(seq_along(dimnames)),
             list(seq_along(dimnames)))
  for (ll in seq_along(excls)) {
    excl <- excls[[ll]]
    dimnames_tmp <- dimnames
    dimnames_tmp[excl] <- list(NULL)
    dimnames(x0) <- dimnames_tmp
    dimnames(x) <- dimnames_tmp
    print(x)
    stopifnot(identical(dim(x), dim(x0)))
    stopifnot(identical(dimnames(x), dimnames(x0)))
    stopifnot(identical(names(x), names))
    y <- as.list(x)
    stopifnot(identical(y, x0))
    z <- as.listenv(y)
    stopifnot(all.equal(z, x))
  } ## for (ll ...)
} ## for (kk ...)


# Assign names
x <- as.listenv(1:6)
dim(x) <- c(2, 3)
dimnames(x) <- lapply(dim(x), FUN = function(n) letters[seq_len(n)])
names(x) <- letters[seq_along(x)]
print(x)
stopifnot(!is.null(dim(x)))
stopifnot(!is.null(dimnames(x)))
stopifnot(!is.null(names(x)))
stopifnot(x[["b"]] == 2L)
stopifnot(x[["a", "b"]] == 3L)

## Extract single element
message("* y <- x[[i,j]] and z <- x[i,j] ...")
dim(x) <- c(2, 3)
dimnames(x) <- list(c("a", "b"), NULL)

y <- x[[3]]
stopifnot(identical(y, 3L))
z <- x[3]
stopifnot(identical(z[[1]], y))

y <- x[[1, 1]]
stopifnot(identical(y, x[[1]]))
z <- x[1, 1]
stopifnot(identical(z[[1]], y))

y <- x[[2, 3]]
stopifnot(identical(y, x[[6]]))
z <- x[2, 3]
stopifnot(identical(z[[1]], y))

y <- x[["a", 3]]
stopifnot(identical(y, x[[1, 3]]))
stopifnot(identical(y, x[[5]]))
z <- x["a", 3]
stopifnot(identical(z[[1]], y))


y <- x[[1, c(FALSE, FALSE, TRUE)]]
stopifnot(identical(y, x[[1, 3]]))
stopifnot(identical(y, x[[5]]))
z <- x[1, c(FALSE, FALSE, TRUE)]
stopifnot(identical(z[[1]], y))


message("* x[[i,j]] <- value ...")
## Assign single element
x[[3]] <- -x[[3]]
stopifnot(identical(x[[3]], -3L))

x[[1, 1]] <- -x[[1, 1]]
stopifnot(identical(x[[1]], -1L))

x[[2, 3]] <- -x[[2, 3]]
stopifnot(identical(x[[6]], -6L))

x[["a", 3]] <- -x[["a", 3]]
stopifnot(identical(x[[1, 3]], -5L))


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Multi-element subsetting
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("* x[i], x[i,j] ...")
x <- as.listenv(1:24)
dim(x) <- c(2, 3, 4)
names(x) <- letters[seq_along(x)]
x[2] <- list(NULL)
print(x)

y <- x[]
print(y)
stopifnot(length(y) == length(x))
stopifnot(all.equal(y, x))
stopifnot(!identical(y, x))
stopifnot(all.equal(as.list(y), as.list(x)[]))

y <- x[1]
print(y)
stopifnot(all.equal(as.list(y), as.list(x)[1]))

y <- x[2:3]
print(y)
stopifnot(all.equal(as.list(y), as.list(x)[2:3]))

y <- x[-1]
print(y)
stopifnot(all.equal(as.list(y), as.list(x)[-1]))

y <- x[1:2, 1:3, 1:4]
print(y)
stopifnot(all.equal(dim(y), dim(x)))
stopifnot(all.equal(y, x))
stopifnot(all.equal(unlist(y), unlist(x)))
stopifnot(all.equal(as.list(y), as.list(x)[1:2, 1:3, 1:4],
                    check.attributes = FALSE))

y <- x[0, 0, 0]
print(y)
stopifnot(length(y) == 0)
stopifnot(all.equal(dim(y), c(0, 0, 0)))
stopifnot(all.equal(y, as.list(x)[0, 0, 0]))

y <- x[0, , ]
print(y)
stopifnot(length(y) == 0)
stopifnot(all.equal(dim(y), c(0, dim(x)[-1])))
stopifnot(all.equal(y, as.list(x)[0, , ]))

y <- x[2, 1, , drop = FALSE]
print(y)
stopifnot(all.equal(dim(y), c(1, 1, dim(x)[3])))
stopifnot(all.equal(as.list(y), as.list(x)[2, 1, , drop = FALSE],
                    check.attributes = FALSE))

y <- x[2, 1, , drop = TRUE]
print(y)
stopifnot(is.null(dim(y)))
stopifnot(all.equal(as.list(y), as.list(x)[2, 1, , drop = TRUE],
                    check.attributes = FALSE))

y <- x[2, 1, ]
print(y)
stopifnot(is.null(dim(y)))
stopifnot(all.equal(as.list(y), as.list(x)[2, 1, ],
                    check.attributes = FALSE))

y <- x[-1, , c(3, 3, 1)]
print(y)
stopifnot(all.equal(as.list(y), as.list(x)[-1, , c(3, 3, 1)],
                    check.attributes = FALSE))

message("* x[i], x[i,j] ... DONE")


message("* x[i] <- value, x[i,j] <- value ...")
dim <- c(2, 3)
n <- prod(dim)
names <- letters[seq_len(n)]

x0 <- as.list(1:n)
dim(x0) <- dim
names(x0) <- names

x <- as.listenv(1:n)
dim(x) <- dim
names(x) <- names

x0[] <- 6:1
x[] <- 6:1
stopifnot(all(unlist(x) == unlist(x0)))

x0[1, ] <- 1:3
x[1, ] <- 1:3
stopifnot(all(unlist(x) == unlist(x0)))

x0[, -2] <- 1:2
x[, -2] <- 1:2
stopifnot(all(unlist(x) == unlist(x0)))

message("* x[i] <- value, x[i,j] <- value ... DONE")

message("* Dropping dimensions from matrix/array by assigning NULL ...")

message("- Dropping rows and columns from matrix")
x <- as.list(1:12)
dim(x) <- c(4, 3)
dimnames(x) <- list(letters[1:4], LETTERS[1:3])
y <- as.listenv(x)

x <- x[, -2, drop = FALSE]
print(x)
y[, 2] <- NULL
print(as.list(y))
stopifnot(identical(as.list(y), x))

## Drop every 2nd row using logical select
x <- x[c(FALSE, TRUE), , drop = FALSE]
print(x)
y[c(TRUE, FALSE), ] <- NULL
print(as.list(y))
stopifnot(identical(as.list(y), x))

## Drop by row names
x <- x["b", , drop = FALSE]
print(x)
y["d", ] <- NULL
print(as.list(y))
stopifnot(identical(as.list(y), x))

## Dropping multiple, duplicated indices
x <- as.list(1:12)
dim(x) <- c(4, 3)
dimnames(x) <- list(letters[1:4], LETTERS[1:3])
y <- as.listenv(x)

x <- x[3:4, 1, drop = FALSE]
print(x)
y[, 3:2] <- NULL
y[c(1, 1, 1, 2), ] <- NULL
print(as.list(y))
stopifnot(identical(as.list(y), x))

message("- Dropping dimensions from array")
x <- as.list(1:12)
dim(x) <- c(2, 2, 3)
dimnames(x) <- list(c("u", "v"), c("a", "b"), c("A", "B", "C"))
print(x)
y <- as.listenv(x)
print(y)

x <- x[-1, , -3, drop = FALSE]
print(x)
y[1, , ] <- NULL
y[, , 3] <- NULL
print(as.list(y))
stopifnot(identical(as.list(y), x))

message("* Dropping dimensions from matrix/array by assigning NULL ... DONE")


message("* dim(x) <- dim on length(x) == 0 ...")
x <- listenv()
dim(x) <- c(2, 3)
stopifnot(length(x) == 6, nrow(x) == 2, ncol(x) == 3)

message("* dim(x) <- n on length(x) == n ...")
x <- as.listenv(1:3)
dim(x) <- 3
dimnames(x) <- list(letters[1:3])
print(x)
stopifnot(
  length(x) == 3,
  identical(dim(x), 3L),
  identical(dimnames(x), list(letters[1:3])),
  is.null(names(x))
)

message("* Exceptions ...")
x <- listenv()
dim(x) <- c(2, 3)

res <- try(x[[3, 3]], silent = TRUE)
stopifnot(inherits(res, "try-error"))

res <- try(x[3, 3], silent = TRUE)
stopifnot(inherits(res, "try-error"))

res <- try(x[c(-1, 1), 3], silent = TRUE)
stopifnot(inherits(res, "try-error"))

res <- try(x[c(TRUE, TRUE, TRUE), ], silent = TRUE)
stopifnot(inherits(res, "try-error"))

res <- try(dimnames(x) <- NA, silent = TRUE)
stopifnot(inherits(res, "try-error"))

res <- try(dimnames(x) <- list("a", "b", "c"), silent = TRUE)
stopifnot(inherits(res, "try-error"))

res <- try(dimnames(x) <- list("a", NULL), silent = TRUE)
stopifnot(inherits(res, "try-error"))

dimnames(x) <- list(c("a", "b"), NULL)


message("* Changing dim(x) and dimnames(x) ...")
x <- listenv()
x[1:12] <- 1:12
dim(x) <- c(2, 2, 3)
dimnames(x) <- list(c("a", "b"), NULL, NULL)
print(x)
stopifnot(identical(dim(x), c(2L, 2L, 3L)))
stopifnot(identical(dimnames(x), list(c("a", "b"), NULL, NULL)))
x[[2, 1, 2]] <- -x[[2, 1, 2]]
y <- unlist(x)
print(y)

dim(x) <- c(4, 3)
print(x)
stopifnot(identical(dim(x), c(4L, 3L)))
stopifnot(is.null(dimnames(x)))
x[[2, 2]] <- -x[[2, 2]]
y <- unlist(x)
print(y)
stopifnot(identical(y, 1:12))


message("* Removing elements ...")

x <- as.listenv(1:6)
dim(x) <- c(2, 3)
names(x) <- letters[seq_along(x)]
print(x)
x[[3]] <- NULL
print(x)
stopifnot(is.null(dim(x)))
stopifnot(!is.null(names(x)), identical(names(x), c("a", "b", "d", "e", "f")))

message("* List environment and multiple dimensions ... DONE")


## Cleanup
options(oopts)
rm(list = setdiff(ls(envir = globalenv()), ovars), envir = globalenv())
