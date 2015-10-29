x <- listenv(a=2, b=3, c="hello")
print(names(x))
names(x)[2] <- "B"
x$d <- 5:8

y <- as.list(x)
str(y)

z <- as.listenv(y)
