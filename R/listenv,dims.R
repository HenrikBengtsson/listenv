#' @export
dim.listenv <- function(x) attr(x, "dim.")

#' @export
`dim<-.listenv` <- function(x, value) {
  n <- length(x)
  if (!is.null(value)) {
    names <- names(value)
    value <- as.integer(value)
    p <- prod(as.double(value))
    if (p != n) {
      stop(sprintf("dims [product %d] do not match the length of object [%d]", p, n))
    }
    names(value) <- names
  }

  ## Always remove "dimnames" and "names" attributes, cf. help("dim")
  dimnames(x) <- NULL
  names(x) <- NULL

  attr(x, "dim.") <- value
  x
}


#' @export
dimnames.listenv <- function(x) attr(x, "dimnames.")

#' @export
`dimnames<-.listenv` <- function(x, value) {
  dim <- dim(x)
  if (is.null(dim) && !is.null(value)) {
    stop("'dimnames' applied to non-array")
  }
  for (kk in seq_along(dim)) {
    names <- value[[kk]]
    if (is.null(names)) next
    n <- length(names)
    if (n != dim[kk]) {
      stop(sprintf("length of 'dimnames' [%d] not equal to array extent", kk))
    }
  }
  attr(x, "dimnames.") <- value
  x
}


#' @export
is.matrix.listenv <- function(x, ...) {
  dim <- dim(x)
  (length(dim) == 2L)
}


#' @export
is.array.listenv <- function(x, ...) {
  dim <- dim(x)
  !is.null(dim)
}


#' @export
#' @method as.vector listenv
as.vector.listenv <- function(x, mode="any") {
  if (mode == "any") mode <- "list"
  x <- as.list(x)
  if (mode != "list") {
    x <- as.vector(x, mode=mode)
  }
  x
}


#' @export
#' @method as.matrix listenv
#' @method is.matrix listenv
as.matrix.listenv <- function(x, ...) {
  dim <- dim(x)
  if (length(dim) != 2L) {
    dim <- c(length(x), 1L)
    dim(x) <- dim
  }
  x
}