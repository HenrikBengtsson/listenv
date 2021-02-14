library("listenv")

message("*** aperm() ...")

dim <- c(2, 3, 4)
dimnames <- lapply(dim, FUN = function(n) letters[seq_len(n)])
X_truth <- as.list(seq_len(prod(dim)))
dim(X_truth) <- dim
dimnames(X_truth) <- dimnames
X <- as.listenv(X_truth)
stopifnot(identical(as.list(X), X_truth))

for (kk in 1:10) {
  perm <- sample(seq_along(dim), replace = FALSE)
  X_truth <- aperm(X_truth, perm = perm)
  X <- aperm(X, perm = perm)
  stopifnot(identical(as.list(X), X_truth))
}

message("*** aperm() ... DONE")
