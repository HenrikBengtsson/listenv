library("listenv")

message("*** c() ...")

message("*** c() - mimic list behavior ...")

x <- list()
y <- as.listenv(x)

x <- c(x, a = 1)
y <- c(y, a = 1)
str(x)
str(as.list(y))
stopifnot(all.equal(as.list(y), x))

t <- c(b = 2)
x <- c(x, t)
y <- c(y, t)
str(x)
str(as.list(y))
stopifnot(all.equal(as.list(y), x))

t <- c(c = 3, d = 4)
x <- c(x, t)
y <- c(y, t)
str(x)
str(as.list(y))
stopifnot(all.equal(as.list(y), x))

t <- 5:6
x <- c(x, t)
y <- c(y, t)
str(x)
str(as.list(y))
stopifnot(all.equal(as.list(y), x))

t <- list(E = 7, F = 8)
x <- c(x, t)
y <- c(y, t)
str(x)
str(as.list(y))
stopifnot(all.equal(as.list(y), x))

t <- list(G = 9 + c(0, 0.1), H = list(h1 = 10, h2 = letters[1:2]))
x <- c(x, t)
y <- c(y, t)
str(x)
str(as.list(y))
stopifnot(all.equal(as.list(y), x))

t <- NULL
x <- c(x, t)
y <- c(y, t)
str(x)
str(as.list(y))
stopifnot(all.equal(as.list(y), x))

t <- list()
x <- c(x, t)
y <- c(y, t)
str(x)
str(as.list(y))
stopifnot(all.equal(as.list(y), x))

message("*** c() - mimic list behavior ... DONE")


message("*** c() - combine with other listenv:s ...")

y <- listenv()
t <- listenv(A = 1)
y <- c(y, t)
str(as.list(y))
stopifnot(all.equal(as.list(y), as.list(t)))

message("*** c() - combine with other listenv:s ... DONE")


message("*** c() - combine with itself listenv:s ...")

x <- list(A = 1)
t <- list(B = 2)
x <- c(x, t, x, x)
str(as.list(x))

y <- listenv(A = 1)
t <- listenv(B = 2)
y <- c(y, t, y, y)
str(as.list(y))
stopifnot(all.equal(as.list(y), x))

message("*** c() - combine with itself listenv:s ... DONE")

message("*** c() ... DONE")
