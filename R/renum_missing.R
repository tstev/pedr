#' Renumber missing id
#'
#' @param id a numeric or character vector for which the missing codes will be
#'   replaced with NA
#' @param warn a logical indicating wheter to print a warning message with the
#'   codes that will be considered as missing
#'
#' @return a numeric of character vector where the codes for missing ids have
#'   been replaced with NA
#' @export renum_miss
#' @rdname renum_miss
#'
#' @examples
#' set.seed(42L)
#' ids <- rpois(50, 100)
#' ids[sample.int(50, 10)] <- c(0L, -99)[sample.int(2, 10, replace = TRUE)]
#' renum_miss(ids)
renum_miss <- function(id, warn = TRUE) {
  assert_that(is.numeric(id) || is.character(id))

  if(is.numeric(id)) miss <- c(0, -99)
  if(is.character(id)) miss <- c(".", "*", " ", "NA")

  if (warn)
    warning("IDs coded as; ", str_list(miss), " will be considered missing",
            immediate. = TRUE, call. = FALSE)

  replace(id, id %in% miss, NA)
}
