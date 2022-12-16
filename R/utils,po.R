stopf <- function(fmt, ..., call. = TRUE, domain = NULL) {  #nolint
  stop(gettextf(fmt, ...), call. = call., domain = domain)
}

warnf <- function(fmt, ..., call. = TRUE, domain = NULL) {  #nolint
  warning(gettextf(fmt, ...), call. = call., domain = domain)
}
