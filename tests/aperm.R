library("listenv")

message("*** aperm() and t() ...")

for (ndim in 0:5) {
  message("- Number of dimensions: ", ndim)

  if (ndim == 0) {
    n <- 3L
    X_truth <- as.list(seq_len(n))
    names(X_truth) <- letters[seq_len(n)]
  } else {
    dim <- seq_len(ndim) + 2L
    dimnames <- lapply(dim, FUN = function(n) letters[seq_len(n)])
    X_truth <- as.list(seq_len(prod(dim)))
    dim(X_truth) <- dim
    dimnames(X_truth) <- dimnames
  }
  
  X <- as.listenv(X_truth)
  stopifnot(identical(as.list(X), X_truth))
  if (ndim <= 1L) {
    stopifnot(!is.null(names(X)) && !is.null(names(X_truth)))
    stopifnot(identical(names(X), names(X_truth)))
  } else {
    stopifnot(is.null(names(X)) && is.null(names(X_truth)))
  }

  if (ndim > 0) {
    message("- aperm()")
    for (kk in 1:10) {
      perm <- sample(seq_len(ndim), replace = FALSE)
      X_truth <- aperm(X_truth, perm = perm)
      X <- aperm(X, perm = perm)
      stopifnot(identical(as.list(X), X_truth))
    }
  }

  if (ndim <= 2) {
    message("- t()")
    X_truth <- t(X_truth)
    X <- t(X)
    ## For comparision: t(<listenv>) preserves element names
    names(X) <- NULL
    stopifnot(identical(as.list(X), X_truth))
  }
} ## for (ndim ...)

message("*** aperm() and t() ... DONE")

