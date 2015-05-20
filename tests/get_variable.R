library("listenv")

ovars <- ls(envir=globalenv())
oopts <- options(warn=1)

x <- listenv(length=3L)
names(x) <- c("a", "b", "c")
stopifnot(length(x) == 3L)
print(map(x))

var <- get_variable(x, "a")
stopifnot(var == "a")
stopifnot(length(x) == 3L)
print(map(x))

var <- get_variable(x, "b")
stopifnot(var == "b")
stopifnot(length(x) == 3L)
print(map(x))

var <- get_variable(x, "c")
stopifnot(var == "c")
stopifnot(length(x) == 3L)
print(map(x))

var <- get_variable(x, "d")
stopifnot(var == "d")
stopifnot(length(x) == 4L)
print(map(x))

var <- get_variable(x, 4L)
stopifnot(var == "d")
stopifnot(length(x) == 4L)
print(map(x))

x$b <- 2
var <- get_variable(x, "b")
stopifnot(var == "b")
stopifnot(length(x) == 4L)
print(map(x))

var <- get_variable(x, length(x) + 1L)
stopifnot(length(x) == 5L)
print(names(x))
print(map(x))


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Exception handling
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
x <- listenv(length=3L)
names(x) <- c("a", "b", "c")

## Non-existing element
res <- try(var <- get_variable(x, "z", mustExist=TRUE), silent=TRUE)
stopifnot(inherits(res, "try-error"))

## Out-of-bound element
res <- try(var <- get_variable(x, 0L, mustExist=TRUE), silent=TRUE)
stopifnot(inherits(res, "try-error"))

## Out-of-bound element
res <- try(var <- get_variable(x, length(x) + 1L, mustExist=TRUE), silent=TRUE)
stopifnot(inherits(res, "try-error"))

res <- try(var <- get_variable(x, c("a", "b")), silent=TRUE)
stopifnot(inherits(res, "try-error"))

res <- try(var <- get_variable(x, 1+2i), silent=TRUE)
stopifnot(inherits(res, "try-error"))



## Cleanup
options(oopts)
rm(list=setdiff(ls(envir=globalenv()), ovars), envir=globalenv())
