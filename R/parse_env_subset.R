#' Helper function to infer target from expression and environment
#'
#' @param expr An expression.
#' @param envir An environment.
#' @param substitute If TRUE, then the expression is
#'        \code{substitute()}:ed, otherwise not.
#'
#' @return A named list.
#'
#' @export
#' @keywords internal
parse_env_subset <- function(expr, envir=parent.frame(), substitute=TRUE) {
  if (substitute) expr <- substitute(expr)
  code <- paste(deparse(expr), collapse="")

  res <- list(envir=envir, name="", subset=NULL, idx=NA_integer_, exists=NA, code=code)

  if (is.symbol(expr)) {
    ## Variable specified as a symbol
    res$name <- deparse(expr)
  } else if (is.character(expr)) {
    ## Variable specified as a name
    if (length(expr) > 1L) {
      stop(sprintf("Does not specify a single variable, but %d: %s", length(expr), hpaste(sQuote(expr), collapse=", ")), call.=FALSE)
    }
    res$name <- expr
  } else if (is.numeric(expr)) {
    ## Variable specified as a subset of envir
    if (length(expr) > 1L) {
      stop(sprintf("Does not specify a single index, but %d: %s", length(expr), hpaste(sQuote(expr), collapse=", ")), call.=FALSE)
    }
    res$subset <- expr
  } else {
    n <- length(expr)
#    if (n == 1L) {
#    } else if (n != 3L) {
#      stop("Invalid syntax: ", sQuote(code), call.=FALSE)
#    }

    if (n == 1L) {
      res$name <- code
    } else if (n >= 3L) {
      ## Assignment to enviroment via $ and [[
      op <- expr[[1]]
      if (op == "$" && n > 3L) {
        stop("Invalid syntax: ", sQuote(code), call.=FALSE)
      } else if (op != "$" && op != "[[") {
        stop("Invalid syntax: ", sQuote(code), call.=FALSE)
      }

      ## Target
      objname <- deparse(expr[[2]])
      if (!exists(objname, envir=envir, inherits=TRUE)) {
        stop(sprintf("Object %s not found: %s", sQuote(objname), sQuote(code)), call.=FALSE)
      }

      obj <- get(objname, envir=envir, inherits=TRUE)
      if (!is.environment(obj)) {
        stop(sprintf("Subsetting can not be done on a %s; only to an environment: %s", sQuote(mode(obj)), sQuote(code)), call.=FALSE)
      }
      res$envir <- obj

      ## Subset
      subset <- list()
      for (kk in 3:n) {
        subsetKK <- expr[[kk]]
        if (is.symbol(subsetKK)) {
          subsetKK <- deparse(subsetKK)
          if (op == "[[") {
            if (!exists(subsetKK, envir=envir, inherits=TRUE)) {
              stop(sprintf("Object %s not found: %s", sQuote(subsetKK), sQuote(code)), call.=FALSE)
            }
            subsetKK <- get(subsetKK, envir=envir, inherits=TRUE)
          }
        } else if (is.language(subsetKK)) {
          subsetKK <- eval(subsetKK, envir=envir)
        }
        subset <- c(subset, subsetKK)
      }

      res$subset <- subset
    } # if (n == ...)
  } # if (is.symbol(expr))


  ## Validat name, iff any
  name <- res$name
  if (nzchar(name) && !grepl("^[.a-zA-Z]+", name)) stop("Not a valid variable name: ", sQuote(name), call.=FALSE)


  ## Validate subsetting, e.g. x[[1]], x[["a"]], and x$a, iff any
  subset <- res$subset
  if (!is.null(subset)) {
    if (is.list(subset)) {
      nsubset <- length(subset)
      if (nsubset == 0L) {
        stop(sprintf("Subsetting of at least on element is required: %s", sQuote(code)), call.=FALSE)
      } else if (nsubset > 1L) {
        stop(sprintf("Subsetting of more than one dimension is currently not implemented: %s", sQuote(code)), call.=FALSE)
      }
      subset <- subset[[1L]]
    }

    if (is.na(subset)) {
      stop("Invalid subsetting. Subset must not be a missing value.")
    } else if (is.character(subset)) {
      if (!nzchar(subset)) {
        stop("Invalid subset. Subset must not be an empty name.")
      }
    } else if (!is.numeric(subset)) {
      stop(sprintf("Invalid subset of type %s: %s", sQuote(typeof(subset)), sQuote(code)), call.=FALSE)
    }

    ## Special: listenv:s
    envir <- res$envir
    if (inherits(envir, "listenv")) {
      names <- names(envir)
      if (is.numeric(subset)) {
        res$idx <- subset
        res$exists <- (res$idx >= 1 && res$idx <= length(envir))
        res$name <- names[subset]
        if (length(res$name) == 0L) res$name <- ""
      } else if (is.character(subset)) {
        res$idx <- match(subset, names)
        res$exists <- !is.na(res$idx) && !is.na(map(envir)[res$idx])
      }
    }
    if (is.character(subset)) res$name <- subset
  }


  ## Identify index?
  if (is.na(res$idx) && nzchar(res$name) && inherits(res$envir, "listenv")) {
    envir <- res$envir
    res$idx <- match(res$name, names(envir))
    res$exists <- !is.na(res$idx) && !is.na(map(envir)[res$idx])
  }

  ## Validate
  if (is.na(res$idx) && !nzchar(res$name)) {
    stop("Invalid subset: ", sQuote(code), call.=TRUE)
  }

  if (is.na(res$exists)) {
    res$exists <- exists(res$name, envir=res$envir, inherits=TRUE)
  }

  ## Sanity check
  stopifnot(is.environment(res$envir))
  stopifnot(is.character(res$name))
  stopifnot(is.null(res$idx) || is.numeric(res$idx))
  stopifnot(is.logical(res$exists), !is.na(res$exists))

  res
}
