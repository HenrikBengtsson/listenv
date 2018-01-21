#' Drop parts of the dimensions of an array
#'
#' @param x A matrix or an array.
#'
#' @param \ldots Exactly K integer arguments where
#' K = \code{length(dim(x))}.
#' The exception is for matrices, where K == 1 is accepted if the
#' the argument is named either \code{rows} or \code{cols}.
#'
#' @param A subset of \code{x} with \code{length(dim(x))} being preserved.
#'
#' @examples
#' x <- as.list(1:12)
#' dim(x) <- c(4, 3)
#' dimnames(x) <- list(letters[1:4], LETTERS[1:3])
#' y <- as.listenv(x)
#' x <- array_drop(x, cols = 2)
#' print(x)
#' y <- array_drop(y, cols = 2)
#' print(as.list(y))
#'
#' x <- as.list(1:12)
#' dim(x) <- c(2, 2, 3)
#' dimnames(x) <- list(c("u", "v"), c("a", "b"), c("A", "B", "C"))
#' print(x)
#' x <- array_drop(x, 1, NULL, 3)
#' print(x)
#' 
#' @export
array_drop <- function(x, ...) {
  stopifnot(is.array(x))
  idxs <- list(...)
  nidxs <- length(idxs)
  if (nidxs == 0) return(x)

  idxs <- lapply(idxs, FUN = unique)
  idxs_n <- unlist(lapply(idxs, FUN = length), use.names = FALSE)
  if (sum(idxs_n) == 0) return(x)

  dim <- dim(x)
  ndim <- length(dim)
  stopifnot(ndim >= nidxs)

  ## Acknowledge arguments 'rows' and 'cols', if specified
  if (ndim == 2L) {
    keys <- names(idxs)
    if (!is.null(keys)) {
      if (nidxs == 1L) {
        ## Only 'cols' was specified?
        if (keys[1L] == "cols") {
          idxs <- list(rows = NULL, cols = idxs[[1L]])
          nidxs <- 2L
          idxs_n <- c(0L, idxs_n)
        }
      } else if (nidxs == 2L) {
        ## Both 'rows' and 'cols' were specified in reverse order?
        if (keys[1L] == "cols" && keys[2L] == "rows") {
          idxs <- rev(idxs)
        }
      }
    }
  }

  stopifnot(ndim == length(idxs))

  dimnames <- dimnames(x)
  array_idxs <- list(array(seq_along(x), dim = dim))

  idxs_0 <- lapply(dim, FUN = function(n) seq_len(n))
  idxs_drop <- NULL
  for (dd in seq_len(ndim)) {
    idxs_dd <- idxs[[dd]]
    if (length(idxs_dd) == 0) next
    stopifnot(all(idxs_dd >= 1 & idxs_dd <= dim[dd]))
    idxs_t <- idxs_0
    idxs_t[[dd]] <- idxs_dd
    idxs_drop_dd <- do.call(`[`, args = c(array_idxs, idxs_t))
    idxs_drop <- c(idxs_drop, idxs_drop_dd)
    dim[dd] <- dim[dd] - length(idxs_dd)
    dimnames[[dd]] <- dimnames[[dd]][-idxs_dd]
  }
  
  x[idxs_drop] <- NULL
  dim(x) <- dim
  dimnames(x) <- dimnames
  
  x
} ## array_drop()


drop_rows_cols <- function(x, rows = NULL, cols = NULL) {
  stopifnot(is.matrix(x))
  if (length(rows) == 0 && length(cols) == 0) return(x)
  ## return(array_drop(x, rows = rows, cols = cols))
  dim <- dim(x)
  dimnames <- dimnames(x)
  idxs <- array(seq_along(x), dim = dim)

  idxs_drop <- NULL
  
  n <- length(rows)
  if (n > 0) {
    rows <- unique(rows)
    stopifnot(all(rows >= 1 & rows <= dim[1L]))
    idxs_drop <- c(idxs_drop, idxs[rows, ])
    dim[1L] <- dim[1L] - n
    dimnames[[1L]] <- dimnames[[1L]][-rows]
  }
  
  n <- length(cols)
  if (n > 0) {
    cols <- unique(cols)
    stopifnot(all(cols >= 1 & cols <= dim[2L]))
    idxs_drop <- c(idxs_drop, idxs[, cols])
    dim[2L] <- dim[2L] - n
    dimnames[[2L]] <- dimnames[[2L]][-cols]
  }

  x[idxs_drop] <- NULL
  dim(x) <- dim
  dimnames(x) <- dimnames
  
  x
} ## drop_rows_cols()
