stopf <- function(fmt, ..., call. = TRUE, domain = NULL) {  #nolint
  msg <- gettextf(fmt, ...)
  msg <- .makeMessage(msg, domain = domain)
  if (is.call(call.)) {
    call <- call.
  } else if (isTRUE(call)) {
    call <- sys.call(which = -1L)
  } else {
    call <- NULL
  }
  cond <- simpleError(msg, call = call)
  stop(cond)
}

warnf <- function(fmt, ..., call. = TRUE, immediate. = FALSE, domain = NULL) {  #nolint
  msg <- gettextf(fmt, ...)
  ## Cannot tweak 'call' when immediate. = TRUE
  if (isTRUE(immediate.)) {
    warning(msg, call. = call., immediate. = immediate., domain = domain)
  } else {
    msg <- .makeMessage(msg, domain = domain)
    if (is.call(call.)) {
      call <- call.
    } else if (isTRUE(call)) {
      call <- sys.call(which = -1L)
    } else {
      call <- NULL
    }
    cond <- simpleWarning(msg, call = call)
    warning(cond)
  }
}

msgf <- function(fmt, ..., appendLF = FALSE, domain = NULL) {  #nolint
  msg <- gettextf(fmt, ...)
  message(msg, appendLF = appendLF, domain = domain)
}
