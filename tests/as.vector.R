library("listenv")

dims <- list(3, c(3, 1), c(2, 3), c(2, 3, 4))

for (dim in dims) {
  x <- as.list(1:prod(dim))
  if (length(dim) > 1) {
    dim(x) <- dim
    dimnames(x) <- lapply(dim, FUN = function(n) letters[seq_len(n)])
  }
  y <- as.listenv(x)

##  stopifnot(is.vector(x) == is.vector(y))
  stopifnot(is.matrix(x) == is.matrix(y))
  stopifnot(is.array(x) == is.array(y))

  for (mode in c("any", "list", "logical", "integer", "double", "complex",
                 "character", "raw")) {
    message("mode: ", mode)

    ## as.vector():
    y <- as.listenv(x)
    vx <- as.vector(x, mode = mode)
    print(vx)
    vy <- as.vector(y, mode = mode)
    stopifnot(identical(vy, vx))
    stopifnot(is.matrix(vx) == is.matrix(vy))
    stopifnot(is.array(vx) == is.array(vy))

    ## as.matrix():
    y <- as.listenv(x)
    mx <- as.matrix(x)
    print(mx)
    my <- as.matrix(y)
    stopifnot(identical(dim(my), dim(mx)))
    stopifnot(identical(dimnames(my), dimnames(mx)))
    stopifnot(is.matrix(mx) == is.matrix(my))
    stopifnot(is.array(mx) == is.array(my))

    ## as.array():
    y <- as.listenv(x)
    ax <- as.array(x)
    print(ax)
    ay <- as.array(y)
    stopifnot(identical(dim(ay), dim(ax)))
    stopifnot(identical(dimnames(ay), dimnames(ax)))
    stopifnot(is.matrix(ax) == is.matrix(ay))
    stopifnot(is.array(ax) == is.array(ay))
  }
}
