## From R.utils 2.0.2 (2015-05-23)
tempvar <- function(prefix="var", value, envir=parent.frame(), inherits=FALSE) {
  maxTries <- 1e6
  maxInt <- .Machine$integer.max

  ii <- 0L
  while (ii < maxTries) {
    # Generate random variable name
    idx <- sample.int(maxInt, size=1L)
    name <- sprintf("%s%d", prefix, idx)

    # Is it available?
    if (!exists(name, envir=envir, inherits=inherits)) {
      # Assign a value?
      if (!missing(value)) {
        assign(name, value, envir=envir, inherits=inherits)
      }
      return(name)
    }

    # Next try
    ii <- ii + 1L
  }

  # Failed to find a unique temporary variable name
  stop(sprintf("Failed to generate a unique non-existing temporary variable with prefix '%s'", prefix))
} # tempvar()



## From R.utils 2.0.2 (2015-05-23)
hpaste <- function(..., sep="", collapse=", ", lastCollapse=NULL, maxHead=if (missing(lastCollapse)) 3 else Inf, maxTail=if (is.finite(maxHead)) 1 else Inf, abbreviate="...") {
  if (is.null(lastCollapse)) lastCollapse <- collapse

  # Build vector 'x'
  x <- paste(..., sep=sep)
  n <- length(x)

  # Nothing todo?
  if (n == 0) return(x)
  if (is.null(collapse)) return(x)

  # Abbreviate?
  if (n > maxHead + maxTail + 1) {
    head <- x[seq(length=maxHead)]
    tail <- rev(rev(x)[seq(length=maxTail)])
    x <- c(head, abbreviate, tail)
    n <- length(x)
  }

  if (!is.null(collapse) && n > 1) {
    if (lastCollapse == collapse) {
      x <- paste(x, collapse=collapse)
    } else {
      xT <- paste(x[1:(n-1)], collapse=collapse)
      x <- paste(xT, x[n], sep=lastCollapse)
    }
  }

  x
} # hpaste()
