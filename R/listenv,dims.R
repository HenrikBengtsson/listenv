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
      if (n == 0) {
        length(x) <- p
      } else {
        stopf("dims [product %d] do not match the length of object [%d]", p, n)
      }
    }
    names(value) <- names
  }

  ## Always remove "dimnames" and "names" attributes, cf. help("dim")
  dimnames(x) <- NULL
  names(x) <- NULL

  attr(x, "dim.") <- value
  x
}


#' Set the dimension of an object
#'
#' @param x An \R object, e.g. a list environment, a matrix, an array, or
#' a data frame.
#'
#' @param value A numeric vector coerced to integers.
#' If one of the elements is missing, then its value is inferred from the
#' other elements (which must be non-missing) and the length of `x`.
#'
#' @return An object with the dimensions set, similar to what
#' \code{\link[base:dim]{dim(x) <- value}} returns.
#'
#' @examples
#' x <- 1:6
#' dim_na(x) <- c(2, NA)
#' print(dim(x))  ## [1] 2 3
#'
#' @name dim_na
#' @aliases dim_na<-
#' @export
`dim_na<-` <- function(x, value) {
  if (!is.null(value)) {
    value <- as.integer(value)
    nas <- which(is.na(value))
    if (length(nas) > 0) {
      if (length(nas) > 1) {
        stop("Argument 'value' may only have one NA: ",
             sprintf("c(%s)", paste(value, collapse = ", ")))
      }
      value[nas] <- as.integer(length(x) / prod(value[-nas]))
    }
  }
  dim(x) <- value
  invisible(x)
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
      stopf("length of 'dimnames' [%d] not equal to array extent", kk)
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
    x <- as.vector(x, mode = mode)
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
