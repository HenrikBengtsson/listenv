#' Create a list environment
#'
#' @param \dots (optional) Named and/or unnamed objects to be
#' assigned to the list environment.
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
  metaenv <- new.env(parent=parent.frame())
  env <- new.env(parent=metaenv)

  ## Add elements?
  if (nargs > 0L) {
    ## Backward compatibility
    if (nargs == 1L && identical(names[1L], "length")) {
      .Deprecated(msg="Use of x <- listenv(length=n) to allocate a list environment of length n is deprecated. Use x <- listenv(); length(x) <- n instead.")
      length <- args$length
      stopifnot(length >= 0L)
      args <- vector("list", length=length)
      nargs <- length
      names <- NULL
    }
  }

  ## Allocate internal variables
  maps <- sprintf(".listenv_var_%d", seq_len(nargs))
  names(maps) <- names
  for (kk in seq_len(nargs)) {
    assign(maps[kk], value=args[[kk]], envir=env, inherits=FALSE)
  }
  metaenv$.listenv.map <- maps

  assign(".listenv_var_count", nargs, envir=env, inherits=FALSE)

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

  names(res) <- names(x)
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

  if (is.null(dim)) {
    if (n == 0) {
      s <- sprintf("`%s` with 0 elements.\n", class(x)[1L])
    } else {
      if (n == 1) {
        s <- sprintf("`%s` with 1 element", class(x)[1L])
      } else {
        s <- sprintf("`%s` with %d elements", class(x)[1L], n)
      }
      names <- names(x)
      if (is.null(names)) {
        s <- sprintf("%s that are not named.\n", s)
      } else {
        s <- sprintf("%s: %s\n", s, hpaste(sQuote(names)))
      }
    }
  } else {
    dimstr <- sprintf("%d (%s)", n, paste(dim, collapse="x"))
    if (n == 0) {
      s <- sprintf("`%s` with %s elements.\n", class(x)[1L], dimstr)
    } else {
      if (n == 1) {
        s <- sprintf("`%s` with %s element", class(x)[1L], dimstr)
      } else {
        s <- sprintf("`%s` with %s elements", class(x)[1L], dimstr)
      }
      names <- names(x)
      if (is.null(names)) {
        s <- sprintf("%s and elements that are not named.\n", s)
      } else {
        s <- sprintf("%s: %s.\n", s, hpaste(sQuote(names)))
      }
    }
  }
  cat(s)
}

#' Variable name map for elements of list environment
#'
#' @param x A list environment.
#'
#' @return The a named character vector
#'
#' @aliases map.listenv
#' @export
#' @keywords internal
map <- function(x, ...) {
  get(".listenv.map", envir=parent.env(x), inherits=FALSE)
}

`map<-` <- function(x, value) {
  stopifnot(is.character(value))
  assign(".listenv.map", value, envir=parent.env(x), inherits=FALSE)
  invisible(x)
}

#' Number of elements in list environment
#'
#' @param x A list environment.
#'
#' @export
#' @keywords internal
length.listenv <- function(x) {
  length(map(x))
}

#' @export
`length<-.listenv` <- function(x, value) {
  map <- map(x)
  n <- length(map)
  value <- as.numeric(value)

  if (value < 0) stop("invalid value")

  ## Nothing to do?
  if (value == n) return(invisible(x))

  ## Expand or shrink?
  if (value > n) {
    ## Add place holders for added elements
    extra <- rep(NA_character_, times=value-n)
    map <- c(map, extra)
  } else {
    ## Drop existing variables
    drop <- (value+1):n
    var <- map[drop]
    ## Some may be internal place holders
    var <- var[!is.na(var)]
    if (length(var) > 0) remove(list=var, envir=x, inherits=FALSE)
    map <- map[-drop]
  }
  map(x) <- map

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
  names(map(x))
}

#' @export
`names<-.listenv` <- function(x, value) {
  map <- map(x)
  if (is.null(value)) {
  } else if (length(value) != length(map)) {
    stop(sprintf("Number of names does not match the number of elements: %s != %s", length(value), length(map)))
  }
##  if (any(duplicated(value))) {
##    stop("Environments cannot have duplicate names on elements")
##  }
  names(map) <- value
  map(x) <- map
  invisible(x)
}

#' List representation of a list environment
#'
#' @param x A list environment.
#' @param all.names If \code{TRUE}, variable names starting with
#'        a period are included, otherwise not.
#' @param sorted If \code{TRUE}, elements are ordered by their names
#'        before being compared, otherwise not.
#' @param ... Not used.
#'
#' @return A list.
#'
#' @export
#' @keywords internal
as.list.listenv <- function(x, all.names=TRUE, sorted=FALSE, ...) {
  vars <- map(x)
  names <- names(x)

  ## Drop names starting with a period
  if (!all.names) {
    keep <- !grepl("^[.]", names)
    vars <- vars[keep]
    names <- names[keep]
  }

  ## Nothing to do?
  if (length(vars) == 0) {
    return(list())
  }

  ## Sort by names?
  if (sorted) {
    o <- order(names)
    vars <- vars[o]
    names <- names[o]
  }

  ## Collect as a named list
  res <- vector("list", length=length(vars))
  names(res) <- names
  ok <- !is.na(vars)
  res[ok] <- mget(vars[ok], envir=x, inherits=FALSE)

  ## Set dimensions?
  dim <- dim(x)
  if (!is.null(dim)) {
    dim(res) <- dim
    dimnames(res) <- dimnames(x)
  }

  res
}


#' Get elements of list environment
#'
#' @param x A list environment.
#' @param name The name or index of the element to retrieve.
#'
#' @return The value of an element or NULL if the element does not exist
#'
#' @aliases [[.listenv
#' @aliases [.listenv
#' @export
#' @keywords internal
`$.listenv` <- function(x, name) {
#' @keywords internal
  map <- map(x)
  var <- map[name]

  ## Non-existing variable?
  if (is.na(var)) return(NULL)

  get(var, envir=x, inherits=FALSE)
}


## [[i,j,...]] -> [[idx]]
toIndex <- function(x, idxs) {
  nidxs <- length(idxs)

  dim <- dim(x)
  if (is.null(dim)) dim <- length(x)
  ndim <- length(dim)
  if (ndim != nidxs) {
    stop("incorrect number of dimensions")
  }
  dimnames <- dimnames(x)

  ## Indexing scale factor per dimension
  scale <- c(1L, cumprod(dim[-ndim]))

  ## Subset
  idx <- 1
  for (kk in 1:nidxs) {
    i <- idxs[[kk]]
    ni <- length(i)
    if (is.character(i)) {
      if (ni != 1L) stop("attempt to select more than one element")
      name <- i
      i <- match(name, table=dimnames[[kk]])
      if (is.na(i)) stop("subscript out of bounds")
    } else if (is.logical(i)) {
      d <- dim[kk]
      i <- rep(i, length.out=d)
      i <- which(i)
    } else if (is.numeric(i)) {
      if (ni != 1L) stop("attempt to select more than one element")
      d <- dim[kk]
      if (i < 0) stop("attempt to select less than one element")
      if (i > d) stop("subscript out of bounds")
    } else {
      stop("invalid subscript type", sQuote(typeof(i)))
    }
    idx <- idx + scale[kk]*(i - 1)
  } # for (kk ...)

  idx
} # toIndex()


#' @export
`[[.listenv` <- function(x, ...) {
  map <- map(x)
  n <- length(map)

  idxs <- list(...)
  nidxs <- length(idxs)

  ## Subsetting by multiple dimensions?
  if (nidxs > 1L) {
    i <- toIndex(x, idxs)
  } else {
    i <- idxs[[1L]]
    if (is.character(i)) {
      name <- i
      i <- match(name, table=names(map))
      if (is.na(i)) return(NULL)
    } else if (!is.numeric(i)) {
      return(NextMethod("[["))
    }

    if (length(i) != 1L) {
      stop("Subsetting of more than one element at the time is not allowed for listenv's: ", length(i))
    }

    if (i < 1L || i > n) {
      stop(sprintf("Subscript out of bounds [%d,%d]: %d", min(1,n), n, i), call.=FALSE)
    }
  }

  var <- map[i]

  ## Return default (NULL)?
  if (is.na(var) || !exists(var, envir=x, inherits=FALSE)) return(NULL)

  get(var, envir=x, inherits=FALSE)
}


#' @export
`[.listenv` <- function(x, ...) {
  idxs <- list(...)
  nidxs <- length(idxs)
  if (nidxs > 1L) {
    i <- toIndex(x, idxs)
  } else {
    i <- idxs[[1L]]
  }

  map <- map(x)
  nmap <- length(map)
  names <- names(map)

  if (is.null(i)) {
    i <- integer(0L)
  } else if (is.character(i)) {
    name <- i
    i <- match(name, table=names)
  } else if (is.numeric(i)) {
    if (!(all(i > 0) || all(i < 0))) {
      stop("Only 0's may be mixed with negative subscripts")
    }
    if (length(i) > 0L && i[1L] < 0) {
      i <- setdiff(seq_len(nmap), -i)
    }
  } else if (is.logical(i)) {
    if (length(i) < nmap) i <- rep(i, length.out=nmap)
    i <- which(i)
  } else {
    return(NextMethod("["))
  }

  ## Nothing to do?
  ni <- length(i)

  ## Allocate result
  res <- listenv()
  length(res) <- ni
  res <- structure(res, class=class(x))

  ## Nothing to do?
  if (ni == 0L) {
    return(res)
  }

  ## Add names?
  if (!is.null(names)) {
    names2 <- names[i]
    names2[i > nmap] <- ""
    names(res) <- names2
  }

  ## Ignore out-of-range indices
  i <- i[i <= nmap]
  for (kk in seq_along(i)) {
    value <- x[[i[kk]]]
    if (!is.null(value)) res[[kk]] <- value
  }

  res
}


new_variable <- function(envir, value, create=TRUE) {
  count <- get(".listenv_var_count", envir=envir, inherits=FALSE)

  count <- count + 1L
  name <- sprintf(".listenv_var_%f", count)

  if (!missing(value)) {
    assign(name, value, envir=envir, inherits=FALSE)
  }

  if (create) {
    assign(".listenv_var_count", count, envir=envir, inherits=FALSE)
  }

  name
} # new_variable()


assign_by_name <- function(x, name, value) {
  ## Argument 'name':
  if (length(name) == 0L) {
    stop("Cannot assign value. Zero-length name.", call.=FALSE)
  } else if (length(name) > 1L) {
    stop("Cannot assign value. More than one name specified: ", hpaste(name), call.=FALSE)
  } else if (nchar(name) == 0L) {
    stop("Cannot assign value. Empty name specific: ", name, call.=FALSE)
  }

  map <- map(x)
  names <- names(map)

  ## Map to an existing or a new element?
  if (is.element(name, names)) {
    var <- map[name]

    ## A new variable?
    if (is.na(var)) {
      var <- name
      map[name] <- name
      map(x) <- map
    }
  } else {
    var <- name

    ## Append to map
    map <- c(map, var)
    if (is.null(names)) names <- rep("", times=length(map))
    names[length(map)] <- var
    names(map) <- names
    map(x) <- map
  }

  ## Assign value
  assign(var, value, envir=x, inherits=FALSE)

  invisible(x)
} # assign_by_name()


assign_by_index <- function(x, i, value) {
  ## Argument 'i':
  if (length(i) == 0L) {
    stop("Cannot assign value. Zero-length index.", call.=FALSE)
  } else if (length(i) > 1L) {
    stop("Cannot assign value. More than one index specified: ", hpaste(i), call.=FALSE)
  } else if (!is.finite(i)) {
    stop("Cannot assign value. Non-finite index: ", i, call.=FALSE)
  } else if (i < 1L) {
    stop("Cannot assign value. Non-positive index: ", i, call.=FALSE)
  }

  map <- map(x)
  n <- length(map)

  ## Variable name
  var <- map[i]

  ## Non-existing variable?
  if (is.na(var)) {
    ## Expand map?
    if (i > n) {
      extra <- rep(NA_character_, times=i-n)
      map <- c(map, extra)
    }

    ## Create internal variable
    map[i] <- new_variable(x, value=value)

    ## Update map
    map(x) <- map
  } else {
    assign(var, value, envir=x, inherits=FALSE)
  }

  invisible(x)
} # assign_by_index()


remove_by_name <- function(x, name) {
  ## Argument 'name':
  if (length(name) == 0L) {
    stop("Cannot remove element. Zero-length name.", call.=FALSE)
  } else if (length(name) > 1L) {
    stop("Cannot remove element. More than one name specified: ", hpaste(name), call.=FALSE)
  } else if (nchar(name) == 0L) {
    stop("Cannot remove element. Empty name specific: ", name, call.=FALSE)
  }

  map <- map(x)

  ## Position in names map?
  idx <- match(name, names(map))

  ## Nothing to do?
  if (is.na(idx)) return(invisible(x))

  ## Drop internal variable, unless place holder
  var <- map[idx]
  if (!is.na(var)) remove(list=var, envir=x, inherits=FALSE)

  map <- map[-idx]
  map(x) <- map

  invisible(x)
} # remove_by_name()


remove_by_index <- function(x, i) {
  ## Argument 'i':
  if (length(i) == 0L) {
    stop("Cannot remove element. Zero-length index.", call.=FALSE)
  } else if (length(i) > 1L) {
    stop("Cannot remove element. More than one index specified: ", hpaste(i), call.=FALSE)
  } else if (!is.finite(i)) {
    stop("Cannot remove element. Non-finite index: ", i, call.=FALSE)
  } else if (i < 1L) {
    stop("Cannot remove element. Non-positive index: ", i, call.=FALSE)
  }

  map <- map(x)

  ## Nothing to do?
  if (i > length(map)) return(invisible(x))

  ## Drop internal variable, unless place holder
  var <- map[i]
  if (!is.na(var)) remove(list=var, envir=x, inherits=FALSE)

  map <- map[-i]
  map(x) <- map

  invisible(x)
} # remove_by_index()




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
    remove_by_name(x, name=name)
  } else {
    assign_by_name(x, name=name, value=value)
  }
}

#' @export
`[[<-.listenv` <- function(x, ..., value) {
  map <- map(x)
  n <- length(map)

  idxs <- list(...)
  nidxs <- length(idxs)

  ## Subsetting by multiple dimensions?
  if (nidxs > 1L) {
    i <- toIndex(x, idxs)
  } else {
    i <- idxs[[1L]]
    if (is.character(i)) {
      if (is.null(value)) {
        x <- remove_by_name(x, name=i)
      } else {
        x <- assign_by_name(x, name=i, value=value)
      }
      return(invisible(x))
    }
  }

  if (is.numeric(i)) {
    if (is.null(value)) {
      x <- remove_by_index(x, i=i)
    } else {
      x <- assign_by_index(x, i=i, value=value)
    }
  } else {
    stop(sprintf("Subsetted [[<- assignment to listenv's is only supported for names and indices, not %s", mode(i)), call.=FALSE)
  }

  return(invisible(x))
}


#' @export
`[<-.listenv` <- function(x, i, ..., value) {
  idxs <- list(...)
  if (length(idxs) > 0L) {
    stop("Not supported")
  }

  if (is.logical(i)) {
    n <- length(x)
    if (length(i) < n) i <- rep(i, length.out=n)
    i <- which(i)
  }

  ni <- length(i)

  # Nothing to do?
  if (ni == 0L) return(invisible(x))

  nvalue <- length(value)
  if (nvalue == 0L) stop("Replacement has zero length", call.=FALSE)

  if (ni != nvalue) {
    if (ni < nvalue || ni %% nvalue != 0) {
      warning(sprintf("Number of items to replace is not a multiple of replacement length: %d != %d", ni, nvalue), call.=FALSE)
    }
    value <- rep(value, length.out=ni)
    nvalue <- length(value)
  }

  if (is.character(i)) {
    for (kk in seq_len(ni)) {
      x <- assign_by_name(x, name=i[kk], value=value[[kk]])
    }
  } else if (is.numeric(i)) {
    for (kk in seq_len(ni)) {
      x <- assign_by_index(x, i=i[kk], value=value[[kk]])
    }
  } else {
    stop(sprintf("Subsetted [<- assignment to listenv's is only supported for names and indices, not %s", mode(i)), call.=FALSE)
  }
  return(invisible(x))
}


#' @export
#' @method unlist listenv
unlist.listenv <- function(x, recursive=TRUE, use.names=TRUE) {
  x <- as.list(x)
  if (recursive) {
    repeat {
      x <- unlist(x, recursive=TRUE, use.names=use.names)
      idxs <- unlist(lapply(x, FUN=inherits, "listenv"), use.names=FALSE)
      if (length(idxs) == 0L) break
      idxs <- which(idxs)
      if (length(idxs) == 0L) break
      for (ii in idxs) {
        x[[ii]] <- unlist(x[[ii]], recursive=TRUE, use.names=use.names)
      }
    }
    x
  } else {
    unlist(x, recursive=FALSE, use.names=use.names)
  }
}

#' @export
dim.listenv <- function(x) attr(x, "dim.")

#' @export
`dim<-.listenv` <- function(x, value) {
  n <- length(x)
  if (!is.null(value)) {
    value <- as.integer(value)
    p <- prod(as.double(value))
    if (p != n) {
      stop(sprintf("dims [product %d] do not match the length of object [%d]", p, n))
    }
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
#' @method all.equal listenv
all.equal.listenv <- function(target, current, all.names=TRUE, sorted=FALSE, ...) {
  if (identical(target, current)) return(TRUE)

  ## Coerce to lists
  target <- as.list(target, all.names=all.names, sorted=sorted)
  current <- as.list(current, all.names=all.names, sorted=sorted)

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

  all.equal(target=target, current=current, ...)
}
