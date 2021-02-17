#' @export
c.listenv <- function(..., recursive = FALSE) {
  if (recursive) {
    stop("Combining recursively is not supported for list environments")
  }
  
  args <- list(...)
  res <- args[[1]]
  nres <- length(res)

  ## From help("c") we have "... all attributes except names are removed"
  ## but also "Note that methods other than the default are not required to
  ## do this (and they will almost certainly preserve a class attribute)."
  ## Drop dimension information
  attr(res, "dim.") <- NULL
  attr(res, "dimnames.") <- NULL

  args <- args[-1]
  nargs <- length(args)

  ## Nothing to do?
  if (nargs == 0L) return(res)

  ## Special case: In order to append a listenv to itself, we much
  ## turn the duplicates into clones, which we do by as.list()
  isSelf <- vapply(args, FUN = identical, res, FUN.VALUE = FALSE)
  if (any(isSelf)) {
    clone <- as.list(res)
    for (ii in which(isSelf)) args[[ii]] <- clone
    clone <- NULL
  }

  names <- as.list(names(res))
  pos <- nres + 1L
  for (ii in seq_len(nargs)) {
    value <- args[[ii]]
    if (is.null(value)) {
    } else if (is.vector(value) || inherits(value, "listenv")) {
      name_ii <- names(args)[ii]
      names_ii <- names(value)
      if (is.null(names_ii)) {
        names_ii <- rep(name_ii, times = length(value))
      } else if (!is.null(name_ii) && nzchar(name_ii)) {
        names_ii <- paste(name_ii, names_ii, sep = ".")
      }
      for (jj in seq_along(value)) {
        res[[pos]] <- value[[jj]]
        name <- names_ii[jj]
        if (is.null(name)) name <- ""
        names[[pos]] <- name
        pos <- pos + 1L
      }
    } else {
      res[[pos]] <- value
      names[[pos]] <- names(args)[ii]
      pos <- pos + 1L
    }
  }

  hasNames <- !vapply(names, FUN = is.null, FUN.VALUE = TRUE)
  if (any(hasNames)) {
    names[!hasNames] <- list("")
    names <- unlist(names, use.names = FALSE)
    names(res) <- names
  }
  
  res
}
