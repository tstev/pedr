#' List items
#'
#' `str_list()` wraps [base::paste0()] in order to turn a list of character
#' strings into a grammatically correct list without an
#' [Oxford Comma](https://en.wikipedia.org/wiki/Serial_comma) by default.
#' Sue me.
#'
#' @param ... one or more character vectors or coercible to character vector.
#' @param sep a string to seperate elements except last.
#' @param last a string to seperate last element.
#'
#' @return a character string.
#' @export str_list
#'
#' @examples
#' shopping_list <- c("bread", "milk", "eggs")
#' str_list(shopping_list)
str_list <- function(..., sep = ", ", last = " and ") {
  args <- list(...)
  x <- do.call("c", args)
  paste0(c(paste0(x[1L:length(x) - 1L], collapse = sep), x[length(x)]),
         collapse = last)
}
