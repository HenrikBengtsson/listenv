#' Create a list environment
#'
#' @param length The number of NULL elements from start.
#' @param ... The object to coerce and optional arguments.
#'
#' @return An environment of class `listenv`.
#'
#' @aliases as.listenv
#' @export
listenv <- function(length=0L) {
  stopifnot(length >= 0L)
  metaenv <- new.env(parent=parent.frame())
  env <- new.env(parent=metaenv)

  ## Allocate internal variables
  maps <- sprintf("var%004d", seq_len(length))
  for (map in maps) assign(map, value=NULL, envir=env, inherits=FALSE)
  metaenv$.listenv.map <- maps

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
  res <- listenv(length=nx)
  names(res) <- names(x)
  for (kk in seq_len(nx)) {
    res[[kk]] <- x[[kk]]
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
  cat(s)
}

#' Variable name map for elements of list environment
#'
#' @param x A list environment.
#'
#' @return The a named character vector
#'
#' @aliases map.listenv map<- map<-.listenv
#' @export
#' @keywords internal
map <- function(...) UseMethod("map")

#' @export
map.listenv <- function(x, ...) {
  get(".listenv.map", envir=x, inherits=TRUE)
}

#' @export
`map<-` <- function(x, value) UseMethod("map<-")

#' @export
`map<-.listenv` <- function(x, value) {
  stopifnot(is.character(value))
  assign(".listenv.map", value, envir=x, inherits=TRUE)
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
#' @param ... Not used.
#'
#' @return A list.
#'
#' @export
#' @keywords internal
as.list.listenv <- function(x, ...) {
  vars <- map(x)
  res <- vector("list", length=length(vars))
  names(res) <- names(x)
  ok <- !is.na(vars)
  res[ok] <- mget(vars[ok], envir=x, inherits=FALSE)
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


#' @export
`[[.listenv` <- function(x, i, ...) {
  map <- map(x)

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

  n <- length(map)
  if (i < 1L || i > n) {
    stop(sprintf("Subscript out of bounds [%d,%d]: %d", min(1,n), n, i), call.=FALSE)
  }

  var <- map[i]

  ## Return default (NULL)?
  if (is.na(var) || !exists(var, envir=x, inherits=FALSE)) return(NULL)

  get(var, envir=x, inherits=FALSE)
}


#' @export
`[.listenv` <- function(x, i, ...) {
  map <- map(x)
  nmap <- length(map)

  if (is.null(i)) {
    i <- integer(0L)
  } else if (is.character(i)) {
    name <- i
    i <- match(name, table=names(map))
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
  res <- structure(listenv(length=ni), class=class(x))

  ## Nothing to do?
  if (ni == 0L) {
    return(res)
  }

  names <- names(x)[i]
  names[i > nmap] <- ""
  names(res) <- names

  ## Ignore out-of-range indices
  i <- i[i <= nmap]
  for (kk in seq_along(i)) {
    value <- x[[i[kk]]]
    if (!is.null(value)) res[[kk]] <- value
  }

  res
}



assign_by_name <- function(...) UseMethod("assign_by_name")

assign_by_name.listenv <- function(x, name, value) {
  ## Argument 'name':
  if (length(name) == 0L) {
    stop("Cannot assign value. Zero-length name.", call.=FALSE)
  } else if (length(name) > 1L) {
    stop("Cannot assign value. More than one name specified: ", hpaste(name), call.=FALSE)
  } else if (nchar(name) == 0L) {
    stop("Cannot assign value. Empty name specific: ", name, call.=FALSE)
  }

  map <- map(x)

  ## Map to an existing or a new element?
  if (is.element(name, names(map))) {
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
    names(map)[length(map)] <- var
    map(x) <- map
  }

  ## Assign value
  assign(var, value, envir=x, inherits=FALSE)

  invisible(x)
} # assign_by_name()


assign_by_index <- function(...) UseMethod("assign_by_index")

assign_by_index.listenv <- function(x, i, value) {
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

    ## Create internal variable name
    var <- tempvar(value=value, envir=x, inherits=FALSE)
    map[i] <- var

    ## Update map
    map(x) <- map
  } else {
    assign(var, value, envir=x, inherits=FALSE)
  }

  invisible(x)
} # assign_by_index()


remove_by_name <- function(...) UseMethod("remove_by_name")

remove_by_name.listenv <- function(x, name) {
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

  var <- map[idx]
  remove(list=var, envir=x, inherits=FALSE)
  map <- map[-idx]
  map(x) <- map

  invisible(x)
} # remove_by_name()


remove_by_index <- function(...) UseMethod("remove_by_index")

remove_by_index.listenv <- function(x, i) {
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

  var <- map[i]
  remove(list=var, envir=x, inherits=FALSE)
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
`[[<-.listenv` <- function(x, i, ..., value) {
  if (is.character(i)) {
    if (is.null(value)) {
      x <- remove_by_name(x, name=i)
    } else {
      x <- assign_by_name(x, name=i, value=value)
    }
  } else if (is.numeric(i)) {
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
      idxs <- which(unlist(lapply(x, FUN=inherits, "listenv"), use.names=FALSE))
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
