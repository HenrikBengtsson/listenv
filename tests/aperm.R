library("listenv")

message("*** aperm() ...")

dim <- c(2, 3, 4)
dimnames <- lapply(dim, FUN = function(n) letters[seq_len(n)])
x <- seq_len(prod(dim))
X_truth <- array(x, dim = dim, dimnames = dimnames)
X <- x
dim(X) <- dim(X_truth)
dimnames(X) <- dimnames(X_truth)
stopifnot(identical(unlist(X), unlist(X_truth)))

for (kk in 1:10) {
  perm <- sample(seq_along(dim), replace = FALSE)
  X_truth <- aperm(X_truth, perm = perm)
  X <- aperm(X, perm = perm)
  stopifnot(identical(unlist(X), unlist(X_truth)))
}

message("*** aperm() ... DONE")
