#' Renumber missing id
#'
#' `renum_miss()` will replace certain values with NA. For numeric vectors ids
#' coded as `0` or `-99` will be considered as missing or unknown. For character
#' vectors this is extended to include `'.'`, `'*'`, `'NA'` and whitespaces.
#'
#' @param id a numeric or character vector for which the missing codes will be
#'   replaced with NA
#' @param ... other arguments passed to specific methods
#' @return a numeric or character vector
#'
#' @export renum_miss
#' @rdname renum_miss
#'
#' @examples
#' set.seed(42L)
#' ids <- rpois(50, 100)
#' ids[sample.int(50, 10)] <- c(0L, -99)[sample.int(2, 10, replace = TRUE)]
#' renum_miss(ids)
#'
#' ids <- c("A", "B", ".", "D", "*", "NA", "  ", "H")
#' renum_miss(ids)
renum_miss <- function(id, ...) {
  UseMethod("renum_miss", id)
}

#' @rdname renum_miss
#' @export
renum_miss.default <- function(id, ...) {
  stop("id must be a numeric or character vector")
}

#' @rdname renum_miss
#' @method renum_miss character
#' @importFrom stringr str_replace_all
#' @export
renum_miss.character <- function(id, ...) {
  str_replace_all(id, "\\.|\\*|\\s+|NA|0|-99", NA_character_)
}

#' @rdname renum_miss
#' @method renum_miss numeric
#' @export
renum_miss.numeric <- function(id, ...) {
  replace(id, id %in% c(0, -99), NA)
}
