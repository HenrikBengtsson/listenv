library("listenv")

ovars <- ls(envir=globalenv())
if (exists("x")) rm(list="x")
if (exists("y")) rm(list="y")

## - - - - - - - - - - - - - - - - - - - - - - - - - -
## Variable in global/parent environment
## - - - - - - - - - - - - - - - - - - - - - - - - - -
target <- parse_env_subset(x, substitute=TRUE)
str(target)
stopifnot(identical(target$envir, environment()),
          target$name == "x", is.na(target$idx), !target$exists)

target <- parse_env_subset("x", substitute=TRUE)
str(target)
stopifnot(identical(target$envir, environment()),
          target$name == "x", is.na(target$idx), !target$exists)

x <- NULL
target <- parse_env_subset(x, substitute=TRUE)
str(target)
stopifnot(identical(target$envir, environment()),
          target$name == "x", is.na(target$idx), target$exists)

target <- parse_env_subset(y, substitute=TRUE)
str(target)
stopifnot(identical(target$envir, environment()),
          target$name == "y", is.na(target$idx), !target$exists)


## - - - - - - - - - - - - - - - - - - - - - - - - - -
## Environment
## - - - - - - - - - - - - - - - - - - - - - - - - - -
message("*** environment")
x <- new.env()

target <- parse_env_subset(x, substitute=TRUE)
str(target)
stopifnot(identical(target$envir, environment()),
          target$name == "x", is.na(target$idx), target$exists)

target <- parse_env_subset(x$a, substitute=TRUE)
str(target)
stopifnot(identical(target$envir, x), target$name == "a", is.na(target$idx), !target$exists)

target <- parse_env_subset("a", envir=x, substitute=TRUE)
str(target)
stopifnot(identical(target$envir, x), target$name == "a", is.na(target$idx), !target$exists)

target <- parse_env_subset(x[["a"]], substitute=TRUE)
str(target)
stopifnot(identical(target$envir, x), target$name == "a", is.na(target$idx), !target$exists)

target <- parse_env_subset("a", envir=x, substitute=TRUE)
str(target)
stopifnot(identical(target$envir, x), target$name == "a", is.na(target$idx), !target$exists)

res <- try(target <- parse_env_subset(1, substitute=FALSE), silent=TRUE)
stopifnot(inherits(res, "try-error"))

res <- try(target <- parse_env_subset(x[[1]], substitute=TRUE), silent=TRUE)
stopifnot(inherits(res, "try-error"))

x$a <- 1
target <- parse_env_subset(x$a, substitute=TRUE)
str(target)
stopifnot(identical(target$envir, x), target$name == "a", is.na(target$idx), target$exists)


## - - - - - - - - - - - - - - - - - - - - - - - - - -
## List environment
## - - - - - - - - - - - - - - - - - - - - - - - - - -
message("*** listenv")
x <- listenv()

target <- parse_env_subset(x, substitute=TRUE)
str(target)
stopifnot(identical(target$envir, environment()),
          target$name == "x", is.na(target$idx), target$exists)

target <- parse_env_subset(x$a, substitute=TRUE)
str(target)
stopifnot(identical(target$envir, x), target$name == "a", is.na(target$idx), !target$exists)

target <- parse_env_subset(x[["a"]], substitute=TRUE)
str(target)
stopifnot(identical(target$envir, x), target$name == "a", is.na(target$idx), !target$exists)

target <- parse_env_subset("a", envir=x, substitute=TRUE)
str(target)
stopifnot(identical(target$envir, x), target$name == "a", is.na(target$idx), !target$exists)

target <- parse_env_subset(x[[1]], substitute=TRUE)
str(target)
stopifnot(identical(target$envir, x), target$name == "", target$idx == 1, !target$exists)

target <- parse_env_subset(x[[2]], substitute=TRUE)
str(target)
stopifnot(identical(target$envir, x), target$name == "", target$idx == 2, !target$exists)

x$a <- 1
target <- parse_env_subset(x$a, substitute=TRUE)
str(target)
stopifnot(identical(target$envir, x), target$name == "a", target$idx  == 1, target$exists)

target <- parse_env_subset("a", envir=x, substitute=TRUE)
str(target)
stopifnot(identical(target$envir, x), target$name == "a", target$idx  == 1, target$exists)

stopifnot(x$a == 1)
stopifnot(x[[1]] == 1)

target <- parse_env_subset(x[[1]], substitute=TRUE)
str(target)
stopifnot(identical(target$envir, x), target$name == "a", target$idx  == 1, target$exists)


x[[3]] <- 3
target <- parse_env_subset(x[[3]], substitute=TRUE)
str(target)
stopifnot(identical(target$envir, x), target$name == "", target$idx  == 3, target$exists)
stopifnot(x[[3]] == 3)
print(names(x))
stopifnot(identical(names(x), c("a", "", "")))


b <- 1
target <- parse_env_subset(x[[b]], substitute=TRUE)
str(target)
stopifnot(identical(target$envir, x), target$name == "a", target$idx  == 1, target$exists)



## - - - - - - - - - - - - - - - - - - - - - - - - - -
## Multi-dimensional subsetting
## - - - - - - - - - - - - - - - - - - - - - - - - - -
x <- listenv()
length(x) <- 6
dim(x) <- c(2,3)

target <- parse_env_subset(x[[2]], substitute=TRUE)
str(target)
stopifnot(identical(target$envir, x), target$idx == 2, target$exists)

target <- parse_env_subset(x[[1,2]], substitute=TRUE)
str(target)
stopifnot(identical(target$envir, x), target$idx == 1:2, target$exists)

target <- parse_env_subset(x[[1,4]], substitute=TRUE)
str(target)
stopifnot(identical(target$envir, x), target$idx == c(1L,4L), !target$exists)


## - - - - - - - - - - - - - - - - - - - - - - - - - -
## Exception handling
## - - - - - - - - - - - - - - - - - - - - - - - - - -
x <- listenv()

res <- try(target <- parse_env_subset(x[[""]], substitute=TRUE), silent=TRUE)
stopifnot(inherits(res, "try-error"))

res <- try(target <- parse_env_subset("_a", substitute=TRUE), silent=TRUE)
stopifnot(inherits(res, "try-error"))

res <- try(target <- parse_env_subset(1:10, envir=x, substitute=FALSE), silent=TRUE)
stopifnot(inherits(res, "try-error"))

res <- try(target <- parse_env_subset(c("a", "b"), envir=x, substitute=FALSE), silent=TRUE)
stopifnot(inherits(res, "try-error"))

res <- try(target <- parse_env_subset(x[1:10], substitute=TRUE), silent=TRUE)
stopifnot(inherits(res, "try-error"))

res <- try(target <- parse_env_subset(x@a, substitute=TRUE), silent=TRUE)
stopifnot(inherits(res, "try-error"))

## Multidimensional subsetting on 'x' without dimensions
res <- try(target <- parse_env_subset(x[[1,2]], substitute=TRUE), silent=TRUE)
stopifnot(inherits(res, "try-error"))

## Multi-dimensional subsetting
x <- listenv()
length(x) <- 6
dim(x) <- c(2,3)

res <- try(target <- parse_env_subset(x[[0]], substitute=TRUE), silent=TRUE)
stopifnot(inherits(res, "try-error"))

res <- try(target <- parse_env_subset(x[[1,0]], substitute=TRUE), silent=TRUE)
stopifnot(inherits(res, "try-error"))

res <- try(target <- parse_env_subset(x[[1,2,3]], substitute=TRUE), silent=TRUE)
stopifnot(inherits(res, "try-error"))


## Cleanup
rm(list=setdiff(ls(envir=globalenv()), ovars), envir=globalenv())
