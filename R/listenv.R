#' Create a list environment
#'
#' @param \dots (optional) Named and/or unnamed objects to be assigned to the
#' list environment.
#'
#' @return An environment of class `listenv`.
#'
#' @example incl/listenv.R
#'
#' @aliases as.listenv
#' @export
listenv <- function(...) {
  args <- list(...)
  nargs <- length(args)
  names <- names(args)

  ## Allocate empty list environment
  metaenv <- new.env(parent = parent.frame())
  env <- new.env(parent = metaenv)

  ## Allocate internal variables
  maps <- sprintf(".listenv_var_%d", seq_len(nargs))
  names(maps) <- names
  for (kk in seq_len(nargs)) {
    assign(maps[kk], value = args[[kk]], envir = env, inherits = FALSE)
  }
  metaenv[[".listenv.map"]] <- maps

  assign(".listenv_var_count", nargs, envir = env, inherits = FALSE)

  class(env) <- c("listenv", class(env))

  env
}

#' @export
#' @rdname listenv
as.listenv <- function(...) UseMethod("as.listenv")

#' @export
as.listenv.listenv <- function(x, ...) {
  x
}

#' @export
as.listenv.list <- function(x, ...) {
  nx <- length(x)
  res <- listenv()
  length(res) <- nx
  names(res) <- names <- names(x)
  for (kk in seq_len(nx)) {
    value <- x[[kk]]
    if (is.null(value)) value <- list(NULL)
    res[[kk]] <- value
  }

  ## Set dimensions?
  dim <- dim(x)
  if (!is.null(dim)) {
    dim(res) <- dim
    dimnames(res) <- dimnames(x)
    names(res) <- names
  }

  res
}

#' @export
as.listenv.environment <- function(x, ...) {
  as.listenv(as.list(x, ...))
}

#' @export
as.listenv.default <- function(x, ...) {
  as.listenv(as.list(x, ...))
}


#' @export
print.listenv <- function(x, ...) {
  n <- length(x)
  dim <- dim(x)
  ndim <- length(dim)
  names <- names(x)
  dimnames <- dimnames(x)
  class <- class(x)[1L]

  if (ndim <= 1) {
    what <- "vector"
  } else if (ndim == 2) {
    what <- "matrix"
  } else {
    what <- "array"
  }

  s <- sprintf("A %s %s with %d", sQuote(class), what, n)
  if (n == 1) {
    s <- sprintf("%s element", s)
  } else {
    s <- sprintf("%s elements", s)
  }
  if (is.null(names)) {
    s <- sprintf("%s (unnamed)", s)
  } else {
    if (n == 0) {
      s <- sprintf("%s (named)", s)
    } else {
      s <- sprintf("%s (%s)", s, hpaste(sQuote(names)))
    }
  }
  if (ndim > 1) {
    dimstr <- paste(dim, collapse = "x")
    has_dimnames <- !sapply(dimnames, FUN = is.null)
    dimnames_tmp <- sapply(dimnames, FUN = function(x) hpaste(sQuote(x)))

    s <- sprintf("%s arranged in %s", s, dimstr)

    if (ndim == 2) {
      if (is.null(dimnames)) {
        s <- sprintf("%s unnamed rows and columns", s)
      } else {
        if (all(has_dimnames)) {
          s <- sprintf("%s rows (%s) and columns (%s)", s,
                       dimnames_tmp[1L], dimnames_tmp[2L])
        } else if (has_dimnames[1]) {
          s <- sprintf("%s rows (%s) and unnamed columns", s, dimnames_tmp[1L])
        } else if (has_dimnames[2]) {
          s <- sprintf("%s unnamed rows and columns (%s)", s, dimnames_tmp[2L])
        } else {
          s <- sprintf("%s unnamed rows and columns", s)
        }
      }
    } else {
      if (is.null(dimnames)) {
        s <- sprintf("%s unnamed dimensions", s)
      } else {
        dimnames_tmp[!has_dimnames] <- "NULL"
        dimnames_tmp <- sprintf("#%d: %s",
                                seq_along(dimnames_tmp), dimnames_tmp)
        dimnames_tmp <- paste(dimnames_tmp, collapse = "; ")
        if (all(has_dimnames)) {
          s <- sprintf("%s dimensions (%s)", s, dimnames_tmp)
        } else if (!any(has_dimnames)) {
          s <- sprintf("%s unnamed dimensions", s)
        } else {
          s <- sprintf("%s partially named dimensions (%s)", s, dimnames_tmp)
        }
      }
    }
  }

  s <- sprintf("%s.\n", s)
  cat(s)
}

#' Name map for elements of list environment
#'
#' @param x A list environment.
#'
#' @return A named character vector
#'
#' @aliases mapping.listenv
#' @export
#' @keywords internal
mapping <- function(x, ...) {
  get(".listenv.map", envir = parent.env(x), inherits = FALSE)
}

#' @rdname mapping
#' @export
#' @keywords internal
map <- function(x, ...) {
  .Deprecated("mapping()", package = .packageName)
  mapping(x)
}

`mapping<-` <- function(x, value) {
  stop_if_not(is.character(value))
  assign(".listenv.map", value, envir = parent.env(x), inherits = FALSE)
  invisible(x)
}

#' Number of elements in list environment
#'
#' @param x A list environment.
#'
#' @aliases lengths.listenv
#' @export
#' @keywords internal
length.listenv <- function(x) {
  length(mapping(x))
}

## BACKPORT / WORKAROUND:
## lengths() was introduced in R 3.2.0, but only became a generic in R 3.3.0.
## Since this packages is supported on R (>= 3.1.2), declaring above methods
## as S3method() in the NAMESPACE would given an error on R (< 3.2.0).
## Because of this, lengths() is declared as a generic if missing, i.e.
## in R (< 3.2.0).  This will make lengths() for list environments to work
## with R (<= 3.2.0) and R (>= 3.3.0) but not with R 3.2.x versions.
if (!exists("lengths", mode = "function")) {
  lengths <- function(x, use.names = FALSE) UseMethod("lengths")  #nolint
}

#' @export
`length<-.listenv` <- function(x, value) {
  map <- mapping(x)
  n <- length(map)
  value <- as.numeric(value)

  if (value < 0) stopf("Cannot set a negative length: %s", value)

  ## Nothing to do?
  if (value == n) return(invisible(x))

  ## Expand or shrink?
  if (value > n) {
    ## Add place holders for added elements
    extra <- rep(NA_character_, times = value - n)
    map <- c(map, extra)
  } else {
    ## Drop existing elements
    drop <- (value + 1):n
    var <- map[drop]
    ## Some may be internal place holders
    var <- var[!is.na(var)]
    if (length(var) > 0) remove(list = var, envir = x, inherits = FALSE)
    map <- map[-drop]
  }
  mapping(x) <- map

  invisible(x)
}


#' Names of elements in list environment
#'
#' @param x A list environment.
#'
#' @aliases names<-.listenv
#' @export
#' @keywords internal
names.listenv <- function(x) {
  names(mapping(x))
}

#' @export
`names<-.listenv` <- function(x, value) {
  map <- mapping(x)
  if (is.null(value)) {
  } else if (length(value) != length(map)) {
    stopf("The number of names does not match the number of elements: %s != %s",
          length(value), length(map))
  }
  names(map) <- value
  mapping(x) <- map
  invisible(x)
}

#' @exportS3Method lengths listenv
lengths.listenv <- function(x, use.names = TRUE) {  #nolint
  ns <- lapply(x, FUN = length)
  if (length(ns) == 0L) return(integer(0L))
  unlist(ns, use.names = use.names)
}

#' List representation of a list environment
#'
#' @param x A list environment.
#' 
#' @param all.names If `TRUE`, element names starting with a period are
#' included, otherwise not.
#' 
#' @param sorted If `TRUE`, elements are ordered by their names before being
#' compared, otherwise not.
#' 
#' @param ... Not used.
#'
#' @return A list.
#'
#' @export
#' @keywords internal
as.list.listenv <- function(x, all.names = TRUE, sorted = FALSE, ...) {
  vars <- mapping(x)
  nvars <- length(vars)
  names <- names(x)

  ## Drop names starting with a period
  if (!all.names && nvars > 0) {
    keep <- !grepl("^[.]", names)
    vars <- vars[keep]
    names <- names[keep]
    nvars <- length(vars)
  }

  ## Sort by names?
  if (sorted && nvars > 0) {
    o <- order(names)
    vars <- vars[o]
    names <- names[o]
  }

  ## Collect as a named list
  res <- vector("list", length = nvars)
  names(res) <- names

  if (nvars > 0) {
    ok <- !is.na(vars)
    res[ok] <- mget(vars[ok], envir = x, inherits = FALSE)
  }

  ## Set dimensions?
  dim <- dim(x)
  if (!is.null(dim)) {
    dim(res) <- dim
    dimnames(res) <- dimnames(x)
    names(res) <- names
  }

  res
}


#' Get elements of list environment
#'
#' @param x A list environment.
#' 
#' @param name The name or index of the element to retrieve.
#'
#' @return The value of an element or `NULL` if the element does not exist.
#'
#' @aliases [[.listenv
#' @aliases [.listenv
#' @export
#' @keywords internal
`$.listenv` <- function(x, name) {
#' @keywords internal
  map <- mapping(x)
  var <- map[name]

  # Non-existing element?
  if (is.na(var)) return(NULL)

  get(var, envir = x, inherits = FALSE)
}


## [[i,j,...]] -> [[idx]]
to_index <- function(x, idxs) {
  nidxs <- length(idxs)

  dim <- dim(x)
  if (is.null(dim)) dim <- length(x)
  ndim <- length(dim)
  if (nidxs != ndim) {
    stopf("Incorrect number of dimensions: %d != %d", nidxs, ndim)
  }
  dimnames <- dimnames(x)
  idx_dimnames <- dimnames

  ## Indexing scale factor per dimension
  scale <- c(1L, cumprod(dim[-ndim]))

  ## Subset
  idx <- 1
  for (kk in 1:nidxs) {
    i <- idxs[[kk]]
    ni <- length(i)
    if (is.character(i)) {
      name <- i
      i <- match(name, table = dimnames[[kk]])
      if (anyNA(i)) {
        unknown <- name[is.na(i)]
        stopf("Unknown names for dimension #%d: %s",
	      kk, hpaste(sQuote(unknown)))
      }
    } else if (is.logical(i)) {
      d <- dim[kk]
      ni <- length(i)
      if (ni > d) {
        stopf("Logical subscript for dimension #%d too long: %d > %d",
	      kk, ni, d)
      }
      if (ni < d) i <- rep(i, length.out = d)
      i <- which(i)
    } else if (is.numeric(i)) {
      d <- dim[kk]
      if (any(i > d)) {
        stopf("Subscript for dimension #%d out of bounds [%d,%d]",
	      kk, min(1, d), d)
      }
      if (any(i < 0)) {
        if (any(i > 0)) {
          stopf("Only 0's may be mixed with negative subscripts (dimension #%d)", kk)
        }
        ## Drop elements
        i <- setdiff(seq_len(d), -i)
      }
      ## Drop zeros
      i <- i[i != 0]
    } else {
      stopf("Invalid subscript type for dimension #%d: %s",
            kk, sQuote(typeof(i)))
    }

    ## Subset dimnames?
    if (!is.null(idx_dimnames)) {
      dn <- idx_dimnames[[kk]]
      if (!is.null(dn)) idx_dimnames[[kk]] <- dn[i]
    }

    i <- scale[kk] * (i - 1)
    if (kk == 1) {
      idx <- idx + i
    } else {
      idx <- outer(idx, i, FUN = `+`)
    }
  } # for (kk ...)

  ## Sanity check
  dim <- dim(idx)
  ndim <- length(dim)
  if (ndim != nidxs) {
    stopf("INTERNAL ERROR: Incompatible dimensions: %d != %d", ndim, nidxs)
  }

  ## Preserve names(dim)
  names(dim(idx)) <- names(dim(x))

  ## Preserve dimnames
  dimnames(idx) <- idx_dimnames

  idx
}


#' @export
`[[.listenv` <- function(x, ...) {
  map <- mapping(x)
  n <- length(map)

  idxs <- list(...)
  nidxs <- length(idxs)

  ## Subsetting by multiple dimensions?
  if (nidxs > 1L) {
    i <- to_index(x, idxs)
  } else {
    i <- idxs[[1L]]
    if (is.character(i)) {
      name <- i
      i <- match(name, table = names(map))
      if (is.na(i)) return(NULL)
    } else if (!is.numeric(i)) {
      return(NextMethod())
    }

    if (length(i) != 1L) {
      stopf("Subsetting of more than one element at the time is not allowed for listenv's: %s", length(i))  #nolint
    }

    if (i < 1L || i > n) {
      stopf("Subscript out of bounds [%d,%d]: %d",
            min(1, n), n, i, call. = FALSE)
    }
  }

  var <- map[i]

  ## Return default (NULL)?
  if (is.na(var) || !exists(var, envir = x, inherits = FALSE)) return(NULL)

  get(var, envir = x, inherits = FALSE)
}


#' @export
`[.listenv` <- function(x, ..., drop = TRUE) {
  # Need to allow for implicit indices, e.g. x[1,,2]
  idxs <- as.list(sys.call())[-(1:2)]  #nolint
  idxs$drop <- NULL
  nidxs <- length(idxs)

  ## Assert that subsetting has correct shape
  dim <- dim(x)
  ndim <- length(dim)
  if (nidxs > 1 && nidxs != ndim) {
    stopf("Incorrect subsetting. Expected %d dimensions but got %d",
          ndim, nidxs)
  }

  ## Implicitly specified dimensions
  missing <- sapply(idxs, FUN = function(x) {
    is.symbol(x) && identical("", deparse(x))
  })
  if (any(missing)) {
    if (nidxs == ndim) {
      envir <- parent.frame()
      for (kk in seq_len(ndim)) {
        if (missing[kk]) {
          idxs[[kk]] <- seq_len(dim[kk])
        } else {
          idxs[[kk]] <- eval(idxs[[kk]], envir = envir, enclos = baseenv())
        }
      }
    } else if (nidxs == 1) {
      if (ndim == 0) {
        idxs <- list(seq_len(length(x)))
      } else {
        # Special case: Preserve dimensions when x[]
        idxs <- lapply(dim, FUN = function(n) seq_len(n))
        nidxs <- length(idxs)
     }
    }
  } else {
    envir <- parent.frame()
    idxs <- lapply(idxs, FUN = eval, envir = envir, enclos = baseenv())
  }

  if (nidxs <= 1L) {
    i <- idxs[[1L]]
  } else {
    i <- to_index(x, idxs)
  }

  map <- mapping(x)
  nmap <- length(map)
  names <- names(map)

  if (is.null(i)) {
    i <- integer(0L)
  } else if (is.character(i)) {
    name <- i
    i <- match(name, table = names)
  } else if (is.numeric(i)) {
    ## Exclude elements with negative indices?
    if (any(i < 0)) {
      stop_if_not(is.null(dim(i)))
      if (any(i > 0)) {
        stop("Only 0's may be mixed with negative subscripts")
      }
      ## Drop elements
      i <- setdiff(seq_len(nmap), -i)
    }
    ## Drop zeros?
    if (is.null(dim(i))) {
      i <- i[i != 0]
    }
  } else if (is.logical(i)) {
    if (length(i) < nmap) i <- rep(i, length.out = nmap)
    i <- which(i)
  } else {
    return(NextMethod())
  }

  ## Nothing to do?
  ni <- length(i)

  ## Allocate result
  res <- listenv()
  length(res) <- ni
  res <- structure(res, class = class(x))

  if (ni > 0L) {
    # Add names?
    if (!is.null(names)) {
      names2 <- names[i]
      names2[i > nmap] <- ""
      names(res) <- names2
    }

    # Ignore out-of-range indices
    j <- i[i <= nmap]
    for (kk in seq_along(j)) {
      value <- x[[j[kk]]]
      if (!is.null(value)) res[[kk]] <- value
    }
  }

  ## Preserve dimensions?
  dim <- dim(i)
  ndim <- length(dim)
  if (ndim > 1) {
    dimnames <- dimnames(i)

    ## Drop singleton dimensions?
    if (drop) {
      keep <- (dim != 1)
      dim <- dim[keep]
      dimnames <- dimnames[keep]
      ndim <- length(dim)
    }

    if (ndim > 1) {
      names <- names(res)
      dim(res) <- dim
      dimnames(res) <- dimnames
      names(res) <- names
    }
  }

  res
}


new_variable <- function(envir, value, create = TRUE) {
  count <- get(".listenv_var_count", envir = envir, inherits = FALSE)

  count <- count + 1L
  name <- sprintf(".listenv_var_%f", count)

  if (!missing(value)) {
    assign(name, value, envir = envir, inherits = FALSE)
  }

  if (create) {
    assign(".listenv_var_count", count, envir = envir, inherits = FALSE)
  }

  name
}


assign_by_name <- function(x, name, value) {
  ## Argument 'name':
  if (length(name) == 0L) {
    stop("Cannot assign value. Zero-length name.", call. = FALSE)
  } else if (length(name) > 1L) {
    stopf("Cannot assign value. More than one name specified: %s",
         hpaste(sQuote(name)), call. = FALSE)
  } else if (nchar(name) == 0L) {
    stopf("Cannot assign value. Empty name specified: %s", sQuote(name), call. = FALSE)
  }

  map <- mapping(x)
  names <- names(map)

  ## Map to an existing or a new element?
  if (is.element(name, names)) {
    var <- map[name]

    ## A new element?
    if (is.na(var)) {
      var <- name
      map[name] <- name
      mapping(x) <- map
    }
  } else {
    var <- name

    ## Append to map
    map <- c(map, var)
    if (is.null(names)) names <- rep("", times = length(map))
    names[length(map)] <- var
    names(map) <- names
    mapping(x) <- map
  }

  ## Assign value
  assign(var, value, envir = x, inherits = FALSE)

  invisible(x)
}


assign_by_index <- function(x, i, value) {
  ## Argument 'i':
  if (length(i) == 0L) {
    stop("Cannot assign value. Zero-length index.", call. = FALSE)
  } else if (length(i) > 1L) {
    stopf("Cannot assign value. More than one index specified: %s", hpaste(i),
         call. = FALSE)
  } else if (!is.finite(i)) {
    stopf("Cannot assign value. Non-finite index: %s", i, call. = FALSE)
  } else if (i < 1L) {
    stopf("Cannot assign value. Non-positive index: %s", i, call. = FALSE)
  }

  map <- mapping(x)
  n <- length(map)

  ## Element name
  var <- map[i]

  ## Non-existing element?
  if (is.na(var)) {
    ## Expand map?
    if (i > n) {
      extra <- rep(NA_character_, times = i - n)
      map <- c(map, extra)
    }

    ## Create internal variable
    map[i] <- new_variable(x, value = value)

    ## Update map
    mapping(x) <- map
  } else {
    assign(var, value, envir = x, inherits = FALSE)
  }

  invisible(x)
}


remove_by_name <- function(x, name) {
  ## Argument 'name':
  if (length(name) == 0L) {
    stop("Cannot remove element. Zero-length name.", call. = FALSE)
  } else if (length(name) > 1L) {
    stopf("Cannot remove element. More than one name specified: %s",
         hpaste(sQuote(name)), call. = FALSE)
  } else if (nchar(name) == 0L) {
    stopf("Cannot remove element. Empty name specified: %s",
         sQuote(name), call. = FALSE)
  }

  map <- mapping(x)

  ## Position in names map?
  idx <- match(name, names(map))

  ## Nothing to do?
  if (is.na(idx)) return(invisible(x))

  ## Drop internal variable, unless place holder
  var <- map[idx]
  if (!is.na(var)) remove(list = var, envir = x, inherits = FALSE)

  map <- map[-idx]
  mapping(x) <- map

  ## Remove dimensions
  names <- names(x)
  dim(x) <- NULL
  names(x) <- names

  invisible(x)
}


remove_by_index <- function(x, i) {
  ## Argument 'i':
  if (length(i) == 0L) {
    stop("Cannot remove element. Zero-length index.", call. = FALSE)
  } else if (length(i) > 1L) {
    stopf("Cannot remove element. More than one index specified: %s", hpaste(i),
         call. = FALSE)
  } else if (!is.finite(i)) {
    stopf("Cannot remove element. Non-finite index: %s", i, call. = FALSE)
  } else if (i < 1L) {
    stopf("Cannot remove element. Non-positive index: %s", i, call. = FALSE)
  }

  map <- mapping(x)

  ## Nothing to do?
  if (i > length(map)) return(invisible(x))

  ## Drop internal variable, unless place holder
  var <- map[i]
  if (!is.na(var)) remove(list = var, envir = x, inherits = FALSE)

  map <- map[-i]
  mapping(x) <- map

  ## Remove dimensions
  names <- names(x)
  dim(x) <- NULL
  names(x) <- names

  invisible(x)
}




#' Set an element of list environment
#'
#' @param x A list environment.
#' @param name Name or index of element
#' @param value The value to assign to the element
#'
#' @aliases [[<-.listenv
#' @aliases [<-.listenv
#' @export
#' @keywords internal
`$<-.listenv` <- function(x, name, value) {
  if (is.null(value)) {
    remove_by_name(x, name = name)
  } else {
    assign_by_name(x, name = name, value = value)
  }
}

#' @export
`[[<-.listenv` <- function(x, ..., value) {
  map <- mapping(x)

  idxs <- list(...)
  nidxs <- length(idxs)

  ## Subsetting by multiple dimensions?
  if (nidxs > 1L) {
    i <- to_index(x, idxs)
  } else {
    i <- idxs[[1L]]
    if (is.character(i)) {
      if (is.null(value)) {
        x <- remove_by_name(x, name = i)
      } else {
        x <- assign_by_name(x, name = i, value = value)
      }
      return(invisible(x))
    }
  }

  if (is.numeric(i)) {
    if (is.null(value)) {
      x <- remove_by_index(x, i = i)
    } else {
      x <- assign_by_index(x, i = i, value = value)
    }
  } else {
    stopf("Subsetted [[<- assignment to listenv's is only supported for names and indices, not %s", mode(i), call. = FALSE)  #nolint
  }

  return(invisible(x))
}


#' @export
`[<-.listenv` <- function(x, ..., value) {
  ## Need to allow for implicit indices, e.g. x[1,,2]
  idxs <- as.list(sys.call())[-(1:2)]  #nolint
  idxs$value <- NULL
  nidxs <- length(idxs)

  ## Assert that subsetting has correct shape
  dim <- dim(x)
  ndim <- length(dim)
  if (nidxs > 1 && nidxs != ndim) {
    stopf("Incorrect subsetting. Expected %d dimensions but got %d",
          ndim, nidxs)
  }

  ## Implicitly specified dimensions
  missing <- sapply(idxs, FUN = function(x) {
    is.symbol(x) && identical("", deparse(x))
  })

  ## Drop elements from matrix or array, e.g. x[,2] <- NULL?
  if (ndim > 0L && nidxs > 1L && is.null(value)) {
    if (ndim - sum(missing) != 1L) {
      stop("Only one dimension at the time can be dropped when assigning NULL")
    }
    envir <- parent.frame()
    dimnames <- dimnames(x)
    array_idxs <- array(seq_along(x), dim = dim)
    dimnames(array_idxs) <- dimnames
    args <- c(list(array_idxs), idxs, list(drop = FALSE))
    idxs_drop <- do.call(`[`, args = args)
    for (dd in which(!missing)) {
      idxs_dd <- idxs[[dd]]
      idxs_dd <- eval(idxs_dd, envir = envir, enclos = baseenv())
      if (length(idxs_dd) == 0) next
      if (is.logical(idxs_dd)) {
        idxs_dd <- rep(idxs_dd, length.out = dim[dd])
        idxs_dd <- which(idxs_dd)
      } else if (is.character(idxs_dd)) {
        idxs_dd <- unique(idxs_dd)
        idxs_dd <- match(idxs_dd, dimnames[[dd]])
      } else {
        idxs_dd <- unique(idxs_dd)
      }
      stop_if_not(is.numeric(idxs_dd))
      dim[dd] <- dim[dd] - length(idxs_dd)
      dimnames[[dd]] <- dimnames[[dd]][-idxs_dd]
    }
    idxs_drop <- sort(unique(idxs_drop), decreasing = TRUE)
    for (i in idxs_drop) x <- remove_by_index(x, i = i)
    dim(x) <- dim
    dimnames(x) <- dimnames
    return(invisible(x))
  }
  
  if (any(missing)) {
    if (nidxs == ndim) {
      envir <- parent.frame()
      for (kk in seq_len(ndim)) {
        if (missing[kk]) {
          idxs[[kk]] <- seq_len(dim[kk])
        } else {
          idxs[[kk]] <- eval(idxs[[kk]], envir = envir, enclos = baseenv())
        }
      }
    } else if (nidxs == 1) {
      if (ndim == 0) {
        idxs <- list(seq_len(length(x)))
      } else {
        ## Special case: Preserve dimensions when x[]
        idxs <- lapply(dim, FUN = function(n) seq_len(n))
        nidxs <- length(idxs)
     }
    }
  } else {
    envir <- parent.frame()
    idxs <- lapply(idxs, FUN = eval, envir = envir, enclos = baseenv())
  }

  if (nidxs <= 1L) {
    i <- idxs[[1L]]
  } else {
    i <- to_index(x, idxs)
  }

  ni <- length(i)
  if (is.logical(i)) {
    n <- length(x)
    if (ni < n) i <- rep(i, length.out = n)
    i <- which(i)
    ni <- length(i)
  }


  # Nothing to do?
  if (ni == 0L) return(invisible(x))

  if (!is.character(i) && !is.numeric(i)) {
    stopf("Subsetted [<- assignment to listenv's is only supported for names and indices, not %s", mode(i), call. = FALSE)  #nolint
  }
  
  # Remove elements?
  if (is.null(value)) {
    idxs <- unique(i)
    if (is.character(i)) {
      for (i in idxs) x <- remove_by_name(x, name = i)
    } else if (is.numeric(i)) {
      idxs <- sort(idxs, decreasing = TRUE)
      for (i in idxs) x <- remove_by_index(x, i = i)
    }
    return(invisible(x))
  }
  
  nvalue <- length(value)
  if (nvalue == 0L) stop("Replacement has zero length", call. = FALSE)

  if (ni != nvalue) {
    if (ni < nvalue || ni %% nvalue != 0) {
      warnf("Number of items to replace is not a multiple of replacement length: %d != %d", ni, nvalue, call. = FALSE)  #nolint
    }
    value <- rep(value, length.out = ni)
    nvalue <- length(value)
  }

  if (is.character(i)) {
    for (kk in seq_len(ni)) {
      x <- assign_by_name(x, name = i[kk], value = value[[kk]])
    }
  } else if (is.numeric(i)) {
    for (kk in seq_len(ni)) {
      x <- assign_by_index(x, i = i[kk], value = value[[kk]])
    }
  }
  return(invisible(x))
}


#' @export
#' @method unlist listenv
unlist.listenv <- function(x, recursive = TRUE, use.names = TRUE) {  #nolint
  names <- names(x)
  x <- as.list(x)
  names(x) <- names

  if (recursive) {
    repeat {
      x <- unlist(x, recursive = TRUE, use.names = use.names)
      idxs <- unlist(lapply(x, FUN = inherits, "listenv"), use.names = FALSE)
      if (length(idxs) == 0L) break
      idxs <- which(idxs)
      if (length(idxs) == 0L) break
      for (ii in idxs) {
        x[[ii]] <- unlist(x[[ii]], recursive = TRUE, use.names = use.names)
      }
    }
    x
  } else {
    unlist(x, recursive = FALSE, use.names = use.names)
  }
}


#' @export
#' @method all.equal listenv
all.equal.listenv <- function(target, current, all.names = TRUE,  #nolint
                              sorted = FALSE, ...) {
  if (identical(target, current)) return(TRUE)

  ## Coerce to lists
  target <- as.list(target, all.names = all.names, sorted = sorted)
  current <- as.list(current, all.names = all.names, sorted = sorted)

  ## Not all as.list() methods support 'all.names'
  if (!all.names) {
    keep <-
    target <- target[!grepl("^[.]", names(target))]
    current <- current[!grepl("^[.]", names(current))]
  }

  ## Not all as.list() methods support 'sorted'
  if (sorted) {
    target <- target[order(names(target))]
    current <- current[order(names(current))]
  }

  all.equal(target = target, current = current, ...)
}
