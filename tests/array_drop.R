library("listenv")

message("* Dropping elements from matrix/array ...")

message("- matrix")
x <- as.list(1:12)
dim(x) <- c(4, 3)
dimnames(x) <- list(letters[1:4], LETTERS[1:3])
y <- as.listenv(x)

x <- x[, -2, drop = FALSE]
print(x)
y[, 2] <- NULL
print(as.list(y))
stopifnot(identical(as.list(y), x))

message("- array")
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

message("* Dropping elements from matrix/array ... DONE")
