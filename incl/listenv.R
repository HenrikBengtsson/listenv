x <- listenv(c=2, a=3, d="hello")
print(names(x))
names(x)[2] <- "A"
x$b <- 5:8

y <- as.list(x)
str(y)

z <- as.listenv(y)
