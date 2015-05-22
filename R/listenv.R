#' Create a list environment
#'
#' @param length The number of NULL elements from start.
#'
#' @return An environment of class `listenv`.
#'
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
    stop(sprintf("Number of names does not match the number of elments: %s != %s", length(value), length(map)))
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
#' @export
#' @keywords internal
`$.listenv` <- function(x, name) {
#' @keywords internal
##  str(list(method="$<-", name=name))
  map <- map(x)
  var <- map[name]

  ## Non-existing variable?
  if (is.na(var)) return(NULL)

  get(var, envir=x, inherits=FALSE)
}


#' @export
`[[.listenv` <- function(x, i, ...) {
  map <- map(x)

##  str(list(method="[[", i=i))
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


assignByName <- function(...) UseMethod("assignByName")

#' @importFrom R.utils hpaste
assignByName.listenv <- function(x, name, value) {
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
} # assignByName()


assignByIndex <- function(...) UseMethod("assignByIndex")

#' @importFrom R.utils tempvar
#' @importFrom R.utils hpaste
assignByIndex.listenv <- function(x, i, value) {
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
} # assignByIndex()



#' Set an element of list environment
#'
#' @param x A list environment.
#' @param name Name or index of element
#' @param value The value to assign to the element
#'
#' @aliases [[<-.listenv
#' @export
#' @keywords internal
`$<-.listenv` <- function(x, name, value) {
  assignByName(x, name=name, value=value)
}

#' @export
`[[<-.listenv` <- function(x, i, value) {
##  str(list(method="[[<-", i=i, value=value))
  if (is.character(i)) {
    x <- assignByName(x, name=i, value=value)
  } else if (is.numeric(i)) {
    x <- assignByIndex(x, i=i, value=value)
  } else if (is.symbol(i)) {
    ## Can this ever occur? /HB 2015-05-19
    name <- eval(i, envir=parent.frame())
    x <- assignByName(x, name=name, value=value)
  } else {
    stop(sprintf("Subsetted [[<- assignment to listenv's is only supported for names and indices, not %s", mode(i)), call.=FALSE)
  }
  return(invisible(x))
}
