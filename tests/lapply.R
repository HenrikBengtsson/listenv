library("listenv")

message("*** lapply() ...")

x <- as.list(1:6)
names(x) <- letters[seq_along(x)]

y <- as.listenv(x)

z0 <- lapply(x, FUN = function(x) x^2)
z1 <- lapply(y, FUN = function(x) x^2)
stopifnot(identical(z1, z0))

message("*** lapply() ... DONE")



message("*** apply() ...")

x <- matrix(as.list(1:6), nrow = 2)
rownames(x) <- letters[seq_len(nrow(x))]
colnames(x) <- LETTERS[seq_len(ncol(x))]

y <- as.listenv(x)

z0 <- apply(x, MARGIN = 1L, FUN = function(x) sum(unlist(x)))

## FIXME: Implement proper aperm() for listenv
aperm.listenv <- function(a, ...) {
  a <- as.list(a)
  a <- aperm(a, ...)
  as.listenv(a)
}

z1 <- apply(y, MARGIN = 1L, FUN = function(x) sum(unlist(x)))
stopifnot(identical(z1, z0))

message("*** apply() ... DONE")
