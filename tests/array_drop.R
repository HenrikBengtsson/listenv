message("* array_drop() ...")

x <- as.list(1:12)
dim(x) <- c(4, 3)
dimnames(x) <- list(letters[1:4], LETTERS[1:3])
y <- as.listenv(x)

x0 <- x[, -2, drop = FALSE]
print(x0)
x1 <- array_drop(x, cols = 2)
print(x1)

y <- array_drop(y, cols = 2)
print(as.list(y))
stopifnot(identical(as.list(y), x1))

x <- as.list(1:12)
dim(x) <- c(2, 2, 3)
dimnames(x) <- list(c("u", "v"), c("a", "b"), c("A", "B", "C"))
print(x)
x0 <- x[-1, , -3, drop = FALSE]
print(x0)
x1 <- array_drop(x, 1, NULL, 3)
print(x1)
stopifnot(identical(x1, x0))

message("* array_drop() ... DONE")
