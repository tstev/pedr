#' Wrap messages for warnings, error or diagnostic messages
#'
#' `msg_wrap()` is a utility function that can be used when messages intended
#' for [warning()], [stop()] or [message()] get too long. Informative messages
#' are useful for the users but clog the script for a developer.
#'
#' @keywords internal
msg_wrap <- function(...) {
  msg <- paste(..., collapse = " ", sep = " ")
  wrapped <- strwrap(msg, width = getOption("width") - 2)
  paste0(wrapped, collapse = "\n")
}


na_vector <- function(mode, length = 1L) {
  out <- rep(NA, length.out = length)
  do.call(paste0("as.", mode), list(out))
}
