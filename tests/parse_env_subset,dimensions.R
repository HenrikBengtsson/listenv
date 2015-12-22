library("listenv")

ovars <- ls(envir=globalenv())
if (exists("x")) rm(list="x")
if (exists("y")) rm(list="y")

## - - - - - - - - - - - - - - - - - - - - - - - - - -
## Multi-dimensional subsetting
## - - - - - - - - - - - - - - - - - - - - - - - - - -
message("*** parse_env_subset() on multi-dimensional listenv ...")

x <- listenv()
length(x) <- 6
dim(x) <- c(2,3)

target <- parse_env_subset(x[[2]], substitute=TRUE)
str(target)
stopifnot(identical(target$envir, x), target$idx == 2, !target$exists)

target <- parse_env_subset(x[[1,2]], substitute=TRUE)
str(target)
stopifnot(identical(target$envir, x), target$idx == 1:2, !target$exists)

x[[1,2]] <- 1.2
target <- parse_env_subset(x[[1,2]], substitute=TRUE)
str(target)
stopifnot(identical(target$envir, x), target$idx == 1:2, target$exists)

target <- parse_env_subset(x[[1,4]], substitute=TRUE)
str(target)
stopifnot(identical(target$envir, x), target$idx == c(1L,4L), !target$exists)

## Assert that x[[1,4]] is not the same as x[[c(1,4)]]
target <- parse_env_subset(x[[1,4]], substitute=TRUE)
str(target)
target2 <- parse_env_subset(x[[c(1,4)]], substitute=TRUE)
str(target2)
target$code <- target2$code <- NULL
stopifnot(!isTRUE(all.equal(target2, target)))


dimnames(x) <- list(c("a", "b"), c("A", "B", "C"))
print(x)

target <- parse_env_subset(x[["a",2]], substitute=TRUE)
str(target)
stopifnot(identical(target$envir, x), target$idx == 1:2, target$exists)

target <- parse_env_subset(x[["a","B"]], substitute=TRUE)
str(target)
stopifnot(identical(target$envir, x), target$idx == 1:2, target$exists)

message("*** parse_env_subset() on multi-dimensional listenv ... DONE")


## - - - - - - - - - - - - - - - - - - - - - - - - - -
## Exception handling
## - - - - - - - - - - - - - - - - - - - - - - - - - -
message("*** parse_env_subset() on multi-dimensional listenv - exceptions ...")

x <- listenv()

## Multidimensional subsetting on 'x' without dimensions
res <- try(target <- parse_env_subset(x[[1,2]], substitute=TRUE), silent=TRUE)
stopifnot(inherits(res, "try-error"))

## Multi-dimensional subsetting
x <- listenv()
length(x) <- 6
dim(x) <- c(2,3)


## - - - - - - - - - - - - - - - - - - - - - - - - - - -
## FIXME: Should zero indices give parse errors or not?
## - - - - - - - - - - - - - - - - - - - - - - - - - - -
res <- try(target <- parse_env_subset(x[[0]], substitute=TRUE), silent=TRUE)
## stopifnot(inherits(res, "try-error"))

res <- try(target <- parse_env_subset(x[[1,0]], substitute=TRUE), silent=TRUE)
## stopifnot(inherits(res, "try-error"))

res <- try(target <- parse_env_subset(x[[1,2,3]], substitute=TRUE), silent=TRUE)
## stopifnot(inherits(res, "try-error"))

message("*** parse_env_subset() on multi-dimensional listenv - exceptions ... DONE")


## Cleanup
rm(list=setdiff(ls(envir=globalenv()), ovars), envir=globalenv())
