library("listenv")

ovars <- ls(envir=globalenv())
oopts <- options(warn=1)


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Allocation and single-element assignments
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
x <- listenv()
print(x)
print(length(x))
print(names(x))
stopifnot(length(x) == 0)

x$a <- 1
print(x)
print(length(x))
print(names(x))
stopifnot(length(x) == 1)
stopifnot(identical(names(x), c("a")))
stopifnot(identical(x$a, 1), is.null(x$b))

x$b <- 2
print(x)
print(length(x))
print(names(x))
stopifnot(length(x) == 2)
stopifnot(identical(names(x), c("a", "b")))
stopifnot(identical(x$b, 2))

x$a <- 0
print(x)
print(length(x))
print(names(x))
stopifnot(length(x) == 2)
stopifnot(identical(names(x), c("a", "b")))
stopifnot(identical(x[["a"]], 0))

x$"a" <- 1
print(x)
print(length(x))
print(names(x))
stopifnot(length(x) == 2)
stopifnot(identical(names(x), c("a", "b")))
stopifnot(identical(x$a, 1))

x[["a"]] <- 0
print(x)
print(length(x))
print(names(x))
stopifnot(length(x) == 2)
stopifnot(identical(names(x), c("a", "b")))


key <- "b"
x[[key]] <- 3
print(length(x))
print(names(x))
stopifnot(length(x) == 2)
stopifnot(identical(names(x), c("a", "b")))
stopifnot(identical(x$b, 3), identical(x[["b"]], 3), identical(x[[key]], 3))

x[[3]] <- 3.14
print(x)
print(length(x))
print(names(x))
stopifnot(length(x) == 3)
stopifnot(identical(names(x), c("a", "b", "")))
stopifnot(identical(x[[3]], 3.14))

names(x) <- c("a", "b", "c")
stopifnot(length(x) == 3)
stopifnot(identical(names(x), c("a", "b", "c")))
stopifnot(identical(x[[3]], 3.14), identical(x[["c"]], 3.14), identical(x$c, 3.14))


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Removing elements
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
x[["a"]] <- NULL
print(x)
print(length(x))
print(names(x))
stopifnot(length(x) == 2)
stopifnot(identical(names(x), c("b", "c")))

x[[3L]] <- NULL
print(x)
print(length(x))
print(names(x))
stopifnot(length(x) == 2)
stopifnot(identical(names(x), c("b", "c")))

x[[2L]] <- NULL
print(x)
print(length(x))
print(names(x))
stopifnot(length(x) == 1)
stopifnot(identical(names(x), c("b")))

x$b <- NULL
print(x)
print(length(x))
print(names(x))
stopifnot(length(x) == 0)


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Assigning NULL
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
x[2L] <- list(NULL)
print(x)
print(length(x))
print(names(x))
stopifnot(length(x) == 2)
stopifnot(identical(names(x), c("", "")))

x['c'] <- list(NULL)
print(x)
print(length(x))
print(names(x))
stopifnot(length(x) == 3)
stopifnot(identical(names(x), c("", "", "c")))


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Assigning multiple elements at once
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
x <- listenv()
x[c('a', 'b', 'c')] <- 1:3
print(x)
str(as.list(x))
print(length(x))
print(names(x))
stopifnot(length(x) == 3)
stopifnot(identical(names(x), c("a", "b", "c")))
stopifnot(identical(as.list(x), list(a=1L, b=2L, c=3L)))

x[c('c', 'b')] <- 2:3
print(x)
str(as.list(x))
print(length(x))
print(names(x))
stopifnot(length(x) == 3)
stopifnot(identical(names(x), c("a", "b", "c")))
stopifnot(identical(as.list(x), list(a=1L, b=3L, c=2L)))

x[c('a', 'c')] <- 1L
print(x)
str(as.list(x))
print(length(x))
print(names(x))
stopifnot(length(x) == 3)
stopifnot(identical(names(x), c("a", "b", "c")))
stopifnot(identical(as.list(x), list(a=1L, b=3L, c=1L)))

x[c('d', 'e')] <- 4:5
print(x)
str(as.list(x))
print(length(x))
print(names(x))
stopifnot(length(x) == 5)
stopifnot(identical(names(x), c("a", "b", "c", "d", "e")))
stopifnot(identical(as.list(x), list(a=1L, b=3L, c=1L, d=4L, e=5L)))



## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Expanding
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
x <- listenv()
for (ii in 1:3) {
  x[[ii]] <- letters[ii]
  print(x[[ii]])
}
print(x)
names(x) <- sprintf("item%d", seq_along(x))
print(x)

y <- as.list(x)
str(y)
stopifnot(identical(names(y), c("item1", "item2", "item3")))
stopifnot(identical(y[[1]], "a"), identical(y[[2]], "b"), identical(y[[3]], "c"))
x[[2]] <- "B"
stopifnot(identical(x$item2, "B"))


x <- listenv()
x[[1]] <- { 1 }
x[[3]] <- { "Hello world!" }
stopifnot(length(x) == 3)
stopifnot(identical(seq_along(x), seq_len(length(x))))
print(x)
names(x) <- c("a", "b", "c")
print(x)
x$b <- TRUE
stopifnot(identical(x[[1]], 1))
stopifnot(identical(x[[2]], TRUE))
stopifnot(identical(x$b, TRUE))
stopifnot(identical(x[["b"]], TRUE))
y <- as.list(x)
str(y)
stopifnot(length(y) == 3)


## Mixed names and indices
x <- listenv()
x$a <- 1
x[[3]] <- 3
print(names(x))
stopifnot(identical(names(x), c("a", "", "")))

# First element (should be named 'a')
var <- get_variable(x, "a")
stopifnot(var == "a")
var <- get_variable(x, 1)
stopifnot(var == "a")

# Third element (should be a temporary name)
var <- get_variable(x, 3)
stopifnot(var != "c")
names(x) <- c("a", "b", "c")
var <- get_variable(x, 3)
stopifnot(var != "c")
var <- get_variable(x, "c")
stopifnot(var != "c")

## Second element (should become 'b', because it was never used
#                  before it was "named" 'b')
x$b <- 2
var <- get_variable(x, 2)
stopifnot(var == "b")
var <- get_variable(x, "b")
stopifnot(var == "b")


## Names where as.integer(names(x)) are integers
x <- listenv()
x[["1"]] <- 1
x[["3"]] <- 3
print(names(x))
stopifnot(identical(names(x), c("1", "3")))


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Warnings
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
x <- listenv()
x[1:3] <- 1:3
res <- tryCatch(x[1:2] <- 1:4, warning=function(w) {
  class(w) <- "try-warning"
  w
})
stopifnot(inherits(res, "try-warning"))

res <- tryCatch(x[1:3] <- 1:2, warning=function(w) {
  class(w) <- "try-warning"
  w
})
stopifnot(inherits(res, "try-warning"))

res <- tryCatch(x[integer(0L)] <- 1, warning=function(w) {
  class(w) <- "try-warning"
  w
})
stopifnot(!inherits(res, "try-warning"))


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Exception handling
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
x <- listenv(length=3L)
names(x) <- c("a", "b", "c")

res <- try(names(x) <- c("a", "b"), silent=TRUE)
stopifnot(inherits(res, "try-error"))

res <- try(x[[1:2]], silent=TRUE)
stopifnot(inherits(res, "try-error"))

res <- try(x[[0]], silent=TRUE)
stopifnot(inherits(res, "try-error"))

res <- try(x[[length(x)+1]], silent=TRUE)
stopifnot(inherits(res, "try-error"))

res <- try(x[[1+2i]], silent=TRUE)
stopifnot(inherits(res, "try-error"))

res <- try(x[[1+2i]] <- 1, silent=TRUE)
stopifnot(inherits(res, "try-error"))

res <- try(x[[integer(0L)]] <- 1, silent=TRUE)
stopifnot(inherits(res, "try-error"))

res <- try(x[[1:2]] <- 1, silent=TRUE)
stopifnot(inherits(res, "try-error"))

res <- try(x[[Inf]] <- 1, silent=TRUE)
stopifnot(inherits(res, "try-error"))

res <- try(x[[0]] <- 1, silent=TRUE)
stopifnot(inherits(res, "try-error"))

res <- try(x[[-1]] <- 1, silent=TRUE)
stopifnot(inherits(res, "try-error"))

res <- try(x[[character(0L)]] <- 1, silent=TRUE)
stopifnot(inherits(res, "try-error"))

res <- try(x[[c("a", "b")]] <- 1, silent=TRUE)
stopifnot(inherits(res, "try-error"))

res <- try(x[[""]] <- 1, silent=TRUE)
stopifnot(inherits(res, "try-error"))


## Cleanup
options(oopts)
rm(list=setdiff(ls(envir=globalenv()), ovars), envir=globalenv())
