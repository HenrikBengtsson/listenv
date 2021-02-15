#' Transpose a 'listenv' array by permuting its dimensions
#'
#' @usage
#' \method{aperm}{listenv}(a, perm, ...)
#'
#' \method{t}{listenv}(x)
#'
#' @param a,x (listenv) The list environment to be transposed
#'
#' @param perm (integer vector) An index vector of length `dim(a)`
#'
#' @param \ldots Additional arguments passed to [base::aperm()].
#'
#' @return Returns a list environment with permuted dimensions
#'
#' @seealso
#' These functions works like [base::aperm()] and [base::t()].
#'
#' @examples
#' x <- as.listenv(1:6)
#' dim(x) <- c(2, 3)
#' dimnames(x) <- list(letters[1:2], LETTERS[1:3])
#' print(x)
#'
#' x <- t(x)
#' print(x)
#'
#' x <- aperm(x, perm = 2:1)
#' print(x)
#'
#' @aliases t.listenv
#' @export
aperm.listenv <- function(a, perm, ...) {
  dim <- dim(a)
  if (is.null(dim)) {
    stop("Argument 'a' must be a matrix or an array")
  }
  ndim <- length(dim)

  if (length(perm) != ndim) {
    stop("Length of argument 'perm' does not match the dimension of 'a': ", length(perm), " != ", ndim)
  }

  if (any(perm < 1 | perm > ndim)) {
    stop("Argument 'perm' specified dimensions out of range")
  }

  if (anyDuplicated(perm) > 0L) {
    stop("Argument 'perm' must not contain duplicates")
  }

  ## Nothing to do?
  if (all(perm == seq_len(ndim))) return(a)

  ## Remap
  idxs <- seq_len(prod(dim))
  dim(idxs) <- dim
  dimnames(idxs) <- dimnames(a)
  idxs <- aperm(idxs, perm = perm, ...)
  map <- mapping(a)
  map <- map[idxs]
  mapping(a) <- map
  map <- NULL

  attr(a, "dim.") <- dim(idxs)
  attr(a, "dimnames.") <- dimnames(idxs)
  idxs <- NULL
  
  a
}

#' @rdname aperm
#' @export
t.listenv <- function(x) {
  dim <- attr(x, "dim.")
  ndim <- length(dim)
  if (ndim == 0L) {
    attr(x, "dim.") <- c(1L, length(x))
    attr(x, "dimnames.") <- list(NULL, names(x))
  } else if (ndim == 1L) {
    attr(x, "dim.") <- c(1L, dim)
    attr(x, "dimnames.") <- list(NULL, attr(x, "dimnames.")[[1]])
  } else if (ndim == 2L) {
    x <- aperm(x, perm = 2:1)
  } else {
    stop("Argument 'x' is not a matrix")
  }
  x
}
